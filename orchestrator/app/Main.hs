{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as EX
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Unique (newUnique)
import Lucid (renderText)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import System.FilePath
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import System.Timeout (timeout)
import Web.Scotty
import Web.Views.Demandas
import Web.Views.NovaDemanda (paginaNovaDemanda)

import AppState
import Database
import Domain.Protocol
import Domain.Types

-- | Lê um checkbox enviado via POST (navegadores omitem campos desmarcados)
lerCheckbox :: T.Text -> ActionM Bool
lerCheckbox nome = isJust . lookup nome <$> formParams

-- | Gerencia o ciclo de vida de cada Worker conectado via WebSocket
wsHandler :: AppState -> WS.ServerApp
wsHandler state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    rawMsg <- WS.receiveData conn :: IO BL.ByteString
    case Aeson.decode rawMsg of
      Just (HandshakeWorker wid@(WorkerId widTexto) estadoInicial) -> do
        conhecido <- validarWorkerDB (stDb state) widTexto
        if not conhecido
          then do
            TIO.putStrLn $ "[WS] Conexão recusada para worker desconhecido/inativo: " <> widTexto
            WS.sendTextData conn (Aeson.encode (HandshakeRecusado "Worker não cadastrado ou inativo no orquestrador."))
            WS.sendClose conn ("Recusado" :: T.Text)
          else do
            agora <- getCurrentTime
            atualizarHeartbeatWorkerDB (stDb state) widTexto agora
            -- Precisa ocorrer ANTES de registrar o worker no STM: depois do registro
            -- o despachante pode atribuir uma demanda nova a ele, que seria
            -- confundida com uma órfã.
            reconciliarNoHandshake state widTexto estadoInicial
            token <- newUnique
            anterior <- atomically $ registrarWorkerSTM state wid conn estadoInicial token
            -- Reconexão do mesmo worker: encerra a conexão antiga para que o handler
            -- dela não continue alterando o estado do registro novo.
            forM_ anterior $ \antiga -> do
              TIO.putStrLn $ "[WS] Worker " <> widTexto <> " reconectou; encerrando conexão anterior."
              void $ tentar (WS.sendClose antiga ("Substituida por nova conexao" :: T.Text))
            TIO.putStrLn $ "[WS] Worker conectado e autenticado: " <> widTexto <> " | Estado: " <> T.pack (show estadoInicial)
            WS.sendTextData conn (Aeson.encode HandshakeAceito)

            -- Mantém a escuta de heartbeats e garante remoção do STM ao desconectar
            loopMensagensWorker state wid conn
              `EX.finally` do
                atomically $ removerWorkerSTM state wid token
                TIO.putStrLn $ "[WS] Worker desconectado: " <> widTexto

      _ -> do
        WS.sendTextData conn (Aeson.encode (HandshakeRecusado "Handshake inválido."))
        WS.sendClose conn ("Protocolo inválido" :: T.Text)

-- | Tempo máximo sem nenhuma mensagem do worker antes de considerar a conexão
-- morta. O worker envia heartbeat a cada 15 s; 45 s tolera duas perdas.
limiteSilencioWorker :: Int
limiteSilencioWorker = 45 * 1000000

-- | Prazo sem heartbeat após o qual as demandas em andamento de um worker
-- desconectado são consideradas órfãs. Cobre também o reinício do orquestrador:
-- o worker tem esse tempo para reconectar e informar a demanda que está executando.
prazoDemandaOrfa :: NominalDiffTime
prazoDemandaOrfa = 5 * 60

-- | Compara as demandas que o banco diz estarem em andamento no worker com o que
-- ele informou no handshake. As que ele não está executando foram perdidas
-- (ex.: a conexão caiu antes de o resultado ser enviado) e vão para ERRO.
reconciliarNoHandshake :: AppState -> T.Text -> EstadoWorker -> IO ()
reconciliarNoHandshake state widTexto estadoInformado = do
  emAndamento <- listarDemandasEmAndamentoDB (stDb state)
  let executando = case estadoInformado of
        Ocupada (DemandaId d) -> Just d
        Livre -> Nothing
      orfas = [ did | (did, w, _) <- emAndamento, w == widTexto, Just did /= executando ]
  agora <- getCurrentTime
  forM_ orfas $ \did -> do
    let motivo = "O worker " <> widTexto <> " reconectou sem esta demanda em execução; o resultado foi perdido."
    marcarDemandaOrfaDB (stDb state) did motivo agora
    TIO.putStrLn $ "[Reconciliação] Demanda #" <> T.pack (show did) <> " marcada como erro: " <> motivo

-- | Periodicamente marca como ERRO as demandas em andamento cujo worker não está
-- conectado e não envia heartbeat há mais de 'prazoDemandaOrfa'.
loopVarreduraOrfas :: AppState -> IO ()
loopVarreduraOrfas state = forever $ do
  threadDelay (60 * 1000000)
  resultado <- tentar $ do
    agora <- getCurrentTime
    emAndamento <- listarDemandasEmAndamentoDB (stDb state)
    conectados <- atomically $ listarWorkersConectadosSTM state
    let semContato hb = maybe True (\t -> diffUTCTime agora t > prazoDemandaOrfa) hb
        orfas = [ (did, w) | (did, w, hb) <- emAndamento
                           , WorkerId w `notElem` conectados
                           , semContato hb ]
    forM_ orfas $ \(did, w) -> do
      let motivo = "O worker " <> w <> " ficou sem contato por mais de "
                   <> T.pack (show (round (prazoDemandaOrfa / 60) :: Int)) <> " min durante o processamento."
      marcarDemandaOrfaDB (stDb state) did motivo agora
      TIO.putStrLn $ "[Varredura] Demanda #" <> T.pack (show did) <> " marcada como erro: " <> motivo
  case resultado of
    Left e -> TIO.putStrLn $ "[Varredura] Erro na iteração (laço continua): " <> T.pack (show e)
    Right () -> pure ()

-- | Recebe e processa mensagens do worker até a conexão cair ou o worker ficar
-- em silêncio por mais de 'limiteSilencioWorker' (conexão meio aberta).
loopMensagensWorker :: AppState -> WorkerId -> WS.Connection -> IO ()
loopMensagensWorker state wid@(WorkerId widTexto) conn =
  laco `EX.catch` \(e :: EX.SomeException) ->
    TIO.putStrLn $ "[WS] Conexão com " <> widTexto <> " encerrada: " <> T.pack (show e)
  where
    laco = do
      mRaw <- timeout limiteSilencioWorker (WS.receiveData conn :: IO BL.ByteString)
      case mRaw of
        Nothing ->
          TIO.putStrLn $ "[WS] " <> widTexto <> " sem mensagens há "
            <> T.pack (show (limiteSilencioWorker `div` 1000000)) <> " s; encerrando a conexão."
        Just rawData -> processar rawData >> laco
    processar rawData =
      case Aeson.decode rawData of
        Just (HeartbeatWorker estadoAtual) -> do
          agora <- getCurrentTime
          atualizarHeartbeatWorkerDB (stDb state) widTexto agora
          atomically $ aplicarHeartbeatSTM state wid estadoAtual
  
        Just (DemandaAceita did) ->
          atomically $ confirmarDemandaSTM state wid did
        Just (AtualizacaoEtapa (DemandaId did) etapa) -> do
          atualizarEtapaDemandaDB (stDb state) did etapa
          TIO.putStrLn $ "[WS] " <> widTexto <> " atualizou demanda " <> T.pack (show did) <> " -> " <> T.pack (show etapa)
  
        Just (DemandaConcluida (DemandaId did) (CaminhoCompartilhado saida)) -> do
          agora <- getCurrentTime
          finalizarDemandaDB (stDb state) did Finalizado (Just saida) Nothing agora
          atomically $ atualizarEstadoWorkerSTM state wid Livre
          TIO.putStrLn $ "[WS] " <> widTexto <> " concluiu demanda " <> T.pack (show did) <> " em " <> T.pack (show saida)
  
        Just (DemandaFalhou (DemandaId did) motivo) -> do
          agora <- getCurrentTime
          finalizarDemandaDB (stDb state) did (Erro motivo) Nothing (Just motivo) agora
          atomically $ atualizarEstadoWorkerSTM state wid Livre
          TIO.putStrLn $ "[WS] " <> widTexto <> " falhou na demanda " <> T.pack (show did) <> ": " <> motivo
        _ -> pure ()

-- | Executa uma ação capturando exceções síncronas. Exceções assíncronas
-- (ex.: ThreadKilled no encerramento do programa) são propagadas.
tentar :: IO a -> IO (Either EX.SomeException a)
tentar acao = do
  r <- EX.try acao
  case r of
    Left e | Just (ae :: EX.SomeAsyncException) <- EX.fromException e -> EX.throwIO ae
    _ -> pure r

-- | Procura demandas NA_FILA e envia para o primeiro worker Livre conectado.
-- Uma falha em qualquer iteração é registrada e o laço continua.
loopDespachante :: AppState -> IO ()
loopDespachante state = forever $ do
  threadDelay 3000000 -- 3 segundos
  resultado <- tentar (despacharProxima state)
  case resultado of
    Left e -> TIO.putStrLn $ "[Despachante] Erro na iteração (laço continua): " <> T.pack (show e)
    Right () -> pure ()

-- | Uma iteração do despachante: reserva um worker, atribui a demanda no banco
-- e a envia. Se algo falhar no meio, desfaz o que já foi feito.
despacharProxima :: AppState -> IO ()
despacharProxima state = do
  mDemanda <- buscarProximaDemandaFilaDB (stDb state)
  case mDemanda of
    Nothing -> pure ()
    Just demanda -> do
      let DemandaId did = demandaId demanda
      -- Reserva o worker no STM para evitar envio duplo
      mWorkerLivre <- atomically $ do
        mapa <- readTVar (stWorkersAtivos state)
        case filter (\(_, cw) -> cwEstado cw == Livre) (Map.toList mapa) of
          [] -> pure Nothing
          ((wid, cw) : _) -> do
            writeTVar (stWorkersAtivos state)
              (Map.insert wid (cw { cwEstado = Ocupada (demandaId demanda), cwConfirmada = False }) mapa)
            pure (Just (wid, cwConn cw))
      case mWorkerLivre of
        Nothing -> pure () -- Nenhum worker livre no momento
        Just (wid@(WorkerId widTexto), conn) -> do
          agora <- getCurrentTime
          -- Passo 1: atribuição no banco. Se falhar, nada foi enviado e o worker
          -- continua utilizável: libera a reserva e propaga o erro.
          atribuido <- tentar (atribuirDemandaWorkerDB (stDb state) did widTexto agora)
          case atribuido of
            Left e -> do
              atomically $ atualizarEstadoWorkerSTM state wid Livre
              EX.throwIO e
            Right () -> pure ()
          TIO.putStrLn $ "[Despachante] Enviando demanda #" <> T.pack (show did) <> " para " <> widTexto
          -- Passo 2: envio. Se falhar, a conexão provavelmente caiu; devolve a
          -- demanda para a fila. O worker permanece reservado até o handler da
          -- conexão removê-lo do mapa (ou até o próximo heartbeat dele).
          enviado <- tentar (WS.sendTextData conn (Aeson.encode (IniciarDemanda demanda)))
          case enviado of
            Left e -> do
              TIO.putStrLn $ "[Despachante] Falha ao enviar demanda #" <> T.pack (show did)
                <> " para " <> widTexto <> "; devolvendo à fila: " <> T.pack (show e)
              devolverDemandaParaFilaDB (stDb state) did
            Right () -> pure ()
main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- Logs imediatos mesmo quando a saída é redirecionada (arquivo, pipe, journald)
  hSetBuffering stdout LineBuffering

  pool  <- inicializarBanco
             ("dados" </> "orquestrador.db")
             ("orchestrator" </> "sql" </> "schema.sql")
  state <- novoAppState pool

  putStrLn "Orquestrador (HTTP + WS) escutando em 0.0.0.0:12000"

  app <- scottyApp $ routes state

  -- Loop que verifica demandas e aloca para máquinas livres
  void $ forkIO (loopDespachante state)

  -- Loop que marca como erro as demandas de workers que sumiram
  void $ forkIO (loopVarreduraOrfas state)

  -- Warp escuta em todas as interfaces (0.0.0.0) na porta 12000. O timeout de
  -- inatividade do Warp (padrão 30 s) também derruba conexões WebSocket; ele fica
  -- acima de 'limiteSilencioWorker' para que a detecção de worker morto seja a
  -- da aplicação, que registra o motivo no log.
  let configWarp = Warp.setPort 12000 $ Warp.setTimeout 60 Warp.defaultSettings
  Warp.runSettings configWarp $
    websocketsOr WS.defaultConnectionOptions (wsHandler state) app

routes :: AppState -> ScottyM ()
routes state = do
  get "/" $ do
    demandas <- liftIO $ listarDemandasDB (stDb state)
    html $ renderText (paginaDemandas demandas)

  -- Fragmento HTML da lista de demandas, consumido pelo polling do htmx em "/"
  get "/fragmento-demandas" $ do
    demandas <- liftIO $ listarDemandasDB (stDb state)
    html $ renderText (fragmentoListaDemandas demandas)

  get "/nova-demanda" $
    html $ renderText paginaNovaDemanda

  post "/submeter-demanda" $ do
    nome      <- formParam "usuarioNome"
    matricula <- formParam "usuarioMatricula"
    email     <- formParam "usuarioEmail"
    perfil    <- formParam "perfilIPED"
    spNum     <- formParam "spNum"
    spAno     <- formParam "spAno"
    urgente   <- lerCheckbox "urgente"
    exportar  <- lerCheckbox "exportarIPED"

    let solicitante = Usuario
          { usuarioNome      = T.strip nome
          , usuarioMatricula = Matricula (T.strip matricula)
          , usuarioEmail     = Email (T.strip email)
          }
        config  = ConfigProcessamento
          { exportarIPED = exportar
          , perfilIPED   = readPerfilIPED perfil
          }

    agora <- liftIO getCurrentTime
    _ <- liftIO $ inserirDemandaDB (stDb state) solicitante config urgente (SolicitacaoPericial spNum spAno) agora
    redirect "/"

readPerfilIPED :: T.Text -> IPEDProfile
readPerfilIPED "default" = IPEDDefault
readPerfilIPED "blind" = Blind
readPerfilIPED "fastmode" = Fastmode
readPerfilIPED "forensic" = Forensic
readPerfilIPED "pedo" = Pedo
readPerfilIPED "triage" = Triage
readPerfilIPED str = error $ "Perfil IPED desconhecido: " <> (T.unpack str)