{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import qualified Control.Exception as EX
import Control.Monad (forM, forM_, forever, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Configuracao (caminhoWebSocket, intervaloHeartbeatSegundos, portaOrquestrador)
import Domain.Protocol
import Domain.Types
import qualified Network.WebSockets as WS
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile, renameFile)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

-- | Executa uma ação capturando exceções síncronas. Exceções assíncronas são propagadas.
tentar :: IO a -> IO (Either EX.SomeException a)
tentar acao = do
  r <- EX.try acao
  case r of
    Left e | Just (ae :: EX.SomeAsyncException) <- EX.fromException e -> EX.throwIO ae
    _ -> pure r

-- | Envia uma mensagem pela conexão atual, se houver. Como a conexão pode ser
-- trocada por uma reconexão durante a execução de uma demanda, quem executa
-- trabalho longo deve usar esta função em vez de guardar um 'WS.Connection'.
-- Falhas são registradas e não propagadas.
enviarSePossivel :: TVar (Maybe WS.Connection) -> MsgDoWorker -> IO ()
enviarSePossivel varConn msg = do
  mConn <- readTVarIO varConn
  case mConn of
    Nothing -> TIO.putStrLn "[Aviso] Sem conexão com o orquestrador; mensagem não enviada."
    Just conn -> do
      r <- tentar (WS.sendTextData conn (Aeson.encode msg))
      case r of
        Left e -> TIO.putStrLn $ "[Aviso] Falha ao enviar mensagem: " <> T.pack (show e)
        Right () -> pure ()

------------------------------------------------------------------------------
-- Resultados pendentes
--
-- O resultado final de uma demanda ('DemandaConcluida'/'DemandaFalhou') é gravado
-- em disco antes de ser enviado e só é apagado quando o orquestrador responde
-- 'ResultadoRegistrado'. Assim ele sobrevive a quedas de conexão e a reinícios
-- do worker, e é reenviado a cada nova conexão até ser confirmado.
------------------------------------------------------------------------------

arquivoPendente :: FilePath -> DemandaId -> FilePath
arquivoPendente dir (DemandaId did) = dir </> (show did <> ".json")

demandaDoResultado :: MsgDoWorker -> Maybe DemandaId
demandaDoResultado (DemandaConcluida did _) = Just did
demandaDoResultado (DemandaFalhou did _) = Just did
demandaDoResultado _ = Nothing

-- | Grava o resultado em disco e tenta enviá-lo. A gravação usa arquivo temporário
-- + renomeação para que um arquivo pela metade nunca seja lido como resultado.
registrarResultado :: FilePath -> TVar (Maybe WS.Connection) -> DemandaId -> MsgDoWorker -> IO ()
registrarResultado dir varConn did msg = do
  let destino = arquivoPendente dir did
      temporario = destino <> ".tmp"
  gravado <- tentar $ do
    BL.writeFile temporario (Aeson.encode msg)
    renameFile temporario destino
  case gravado of
    Left e -> TIO.putStrLn $ "[Pendentes] Falha ao gravar resultado da demanda "
                <> T.pack (show (unDemandaId did)) <> " em disco (sem reenvio garantido): " <> T.pack (show e)
    Right () -> pure ()
  enviarSePossivel varConn msg

-- | Lê os resultados ainda não confirmados. Arquivos ilegíveis são registrados e ignorados.
listarPendentes :: FilePath -> IO [(DemandaId, MsgDoWorker)]
listarPendentes dir = do
  nomes <- filter (".json" `isSuffixOf`) <$> listDirectory dir
  fmap catMaybes $ forM nomes $ \nome -> do
    conteudo <- tentar (BL.readFile (dir </> nome))
    case fmap Aeson.eitherDecode conteudo of
      Right (Right msg) | Just did <- demandaDoResultado msg -> pure (Just (did, msg))
      outro -> do
        let motivo = case outro of
              Left e -> show e
              Right (Left erro) -> erro
              Right (Right _) -> "não é um resultado de demanda"
        TIO.putStrLn $ "[Pendentes] Ignorando " <> T.pack nome <> ": " <> T.pack motivo
        pure Nothing

removerPendente :: FilePath -> DemandaId -> IO ()
removerPendente dir did = do
  r <- tentar (removeFile (arquivoPendente dir did))
  case r of
    Left e -> TIO.putStrLn $ "[Pendentes] Falha ao remover resultado confirmado: " <> T.pack (show e)
    Right () -> TIO.putStrLn $ "[Pendentes] Resultado da demanda " <> T.pack (show (unDemandaId did)) <> " confirmado pelo orquestrador."

------------------------------------------------------------------------------

-- | Executa o script AutoHotkey de teste e reporta o resultado ao Orquestrador
executarScriptTeste :: FilePath -> TVar (Maybe WS.Connection) -> TVar EstadoWorker -> Demanda -> IO ()
executarScriptTeste dirPendentes varConn varEstado demanda = do
  let did = demandaId demanda
      caminhoAHK = "C:\\Users\\PA\\Desktop\\PA_Auto\\test.ahk"
      -- Se o AutoHotkey.exe não estiver no PATH do Windows, use o caminho completo:
      -- ex: "C:\\Program Files\\AutoHotkey\\v2\\AutoHotkey64.exe"
      executavelAHK = "C:\\Users\\PA\\Desktop\\PA_Auto\\deps\\AutoHotKey\\AutoHotkey64.exe"
      reportar = registrarResultado dirPendentes varConn did

  -- O 'finally' garante a volta a Livre em qualquer saída, inclusive exceções
  -- assíncronas (ex.: killThread), para o worker nunca ficar preso em Ocupada.
  (`EX.finally` atomically (writeTVar varEstado Livre)) $ do
    TIO.putStrLn $ "[Execução] Disparando AutoHotkey: " <> T.pack caminhoAHK

    -- Executa de forma síncrona (aguarda o script terminar). Exceções ao lançar
    -- o processo (ex.: executável inexistente) são capturadas e viram falha.
    resultado <- tentar $
      readCreateProcessWithExitCode (proc executavelAHK [caminhoAHK]) ""

    case resultado of
      Left e -> do
        let motivo = "Falha ao executar o AutoHotkey: " <> T.pack (show e)
        TIO.putStrLn $ "[Execução] " <> motivo
        reportar (DemandaFalhou did motivo)

      Right (ExitSuccess, _stdout, _stderr) -> do
        TIO.putStrLn "[Execução] Script AutoHotkey finalizado com sucesso."
        let saidaTeste = CaminhoCompartilhado "C:\\Users\\PA\\Desktop\\PA_Auto\\saida_teste"
        reportar (DemandaConcluida did saidaTeste)

      Right (ExitFailure codigo, _stdout, stderrStr) -> do
        let motivo = "AutoHotkey retornou erro (código " <> T.pack (show codigo) <> "): " <> T.pack stderrStr
        TIO.putStrLn $ "[Execução] " <> motivo
        reportar (DemandaFalhou did motivo)


-- | Uma sessão com o orquestrador: handshake e laço de comandos. Devolve True se o
-- handshake foi aceito (mesmo que a conexão tenha caído depois). Falhas antes do
-- aceite propagam como exceção.
sessao :: FilePath -> WorkerId -> TVar EstadoWorker -> TVar (Maybe WS.Connection) -> WS.Connection -> IO Bool
sessao dirPendentes wid varEstado varConn conn = do
  estadoAtual <- readTVarIO varEstado
  pendentes <- listarPendentes dirPendentes
  WS.sendTextData conn (Aeson.encode (HandshakeWorker wid estadoAtual (map fst pendentes)))
  mResposta <- timeout 10000000 (WS.receiveData conn :: IO BL.ByteString)
  case mResposta >>= Aeson.decode of
    Just HandshakeAceito -> do
      TIO.putStrLn "[OK] Handshake aceito pelo orquestrador."
      atomically $ writeTVar varConn (Just conn)
      -- Heartbeat em background reportando o estado real do TVar
      hb <- forkIO $ void $ tentar $ forever $ do
        threadDelay (intervaloHeartbeatSegundos * 1000000)
        est <- readTVarIO varEstado
        WS.sendTextData conn (Aeson.encode (HeartbeatWorker est))
      resultado <- tentar (reenviarPendentes pendentes >> comandos conn) `EX.finally` do
        killThread hb
        atomically $ writeTVar varConn Nothing
      case resultado of
        Left e -> TIO.putStrLn $ "[Conexão] Encerrada: " <> T.pack (show e)
        Right () -> TIO.putStrLn "[Conexão] Encerrada."
      pure True
    Just (HandshakeRecusado motivo) -> do
      TIO.putStrLn $ "[ERRO] Conexão recusada: " <> motivo
      WS.sendClose conn ("Saindo" :: T.Text)
      pure False
    _ -> do
      TIO.putStrLn "[ERRO] Resposta inválida ou ausente no handshake."
      WS.sendClose conn ("Saindo" :: T.Text)
      pure False
  where
    reenviarPendentes pendentes =
      forM_ pendentes $ \(DemandaId did, msg) -> do
        TIO.putStrLn $ "[Pendentes] Reenviando resultado da demanda " <> T.pack (show did)
        WS.sendTextData conn (Aeson.encode msg)

    -- Loop de comandos vindos do orquestrador; só termina por exceção
    comandos c = forever $ do
      cmdRaw <- WS.receiveData c :: IO BL.ByteString
      case Aeson.decode cmdRaw of
        Just (IniciarDemanda demanda) -> do
          TIO.putStrLn $ "[CMD] Recebida demanda: " <> T.pack (show (demandaId demanda))
          -- Marca Ocupada ANTES de criar a thread e confirma o recebimento ao
          -- orquestrador, para que heartbeats seguintes nunca informem Livre
          -- com uma demanda em mãos.
          atomically $ writeTVar varEstado (Ocupada (demandaId demanda))
          WS.sendTextData c (Aeson.encode (DemandaAceita (demandaId demanda)))
          -- Roda em thread separada para não bloquear o recebimento de outras mensagens WS
          void $ forkIO $ executarScriptTeste dirPendentes varConn varEstado demanda
        Just (ResultadoRegistrado did) ->
          removerPendente dirPendentes did
        Just (CancelarDemanda did) ->
          TIO.putStrLn $ "[CMD] Cancelamento solicitado para: " <> T.pack (show did)
        Just SolicitarEstado -> do
          est <- readTVarIO varEstado
          WS.sendTextData c (Aeson.encode (HeartbeatWorker est))
        _ ->
          TIO.putStrLn $ "[Aviso] Mensagem inesperada recebida do servidor: " <> T.pack (show cmdRaw)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- Logs imediatos mesmo quando a saída é redirecionada para arquivo ou pipe
  hSetBuffering stdout LineBuffering
  args <- getArgs
  (host, widTexto) <- case args of
    [h, w] -> pure (h, T.pack w)
    _ -> do
      putStrLn "Uso: worker.exe <IP_ORQUESTRADOR> <WORKER_ID>"
      exitFailure
  let wid = WorkerId widTexto
  -- Resultados pendentes ficam ao lado do executável (o diretório de trabalho
  -- de uma tarefa agendada pode ser, por exemplo, C:\Windows\System32).
  dirPendentes <- (</> "resultados-pendentes") . takeDirectory <$> getExecutablePath
  createDirectoryIfMissing True dirPendentes
  varEstado <- newTVarIO Livre
  varConn <- newTVarIO Nothing
  -- Reconecta indefinidamente. A espera dobra a cada falha (até 'esperaMaxima')
  -- e volta ao valor inicial depois de uma sessão aceita pelo orquestrador.
  let esperaInicial = 2 :: Int
      esperaMaxima = 60 :: Int
      loop espera = do
        TIO.putStrLn $ "Conectando ao orquestrador em " <> T.pack host <> ":" <> T.pack (show portaOrquestrador) <> " como " <> widTexto <> "..."
        r <- tentar (WS.runClient host portaOrquestrador caminhoWebSocket (sessao dirPendentes wid varEstado varConn))
        aceito <- case r of
          Right ok -> pure ok
          Left e -> do
            TIO.putStrLn $ "[Conexão] Falha: " <> T.pack (show e)
            pure False
        let espera' = if aceito then esperaInicial else espera
        TIO.putStrLn $ "Reconectando em " <> T.pack (show espera') <> " s..."
        threadDelay (espera' * 1000000)
        loop (min esperaMaxima (espera' * 2))
  loop esperaInicial
