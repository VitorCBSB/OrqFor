{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import qualified Control.Exception as EX
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Time (getCurrentTime)
import Lucid (renderText)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import System.FilePath
import System.IO (hSetEncoding, stderr, stdout, utf8)
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
            atomically $ registrarWorkerSTM state wid conn estadoInicial
            TIO.putStrLn $ "[WS] Worker conectado e autenticado: " <> widTexto <> " | Estado: " <> T.pack (show estadoInicial)
            WS.sendTextData conn (Aeson.encode HandshakeAceito)

            -- Mantém a escuta de heartbeats e garante remoção do STM ao desconectar
            loopMensagensWorker state wid conn
              `EX.finally` do
                atomically $ removerWorkerSTM state wid
                TIO.putStrLn $ "[WS] Worker desconectado: " <> widTexto

      _ -> do
        WS.sendTextData conn (Aeson.encode (HandshakeRecusado "Handshake inválido."))
        WS.sendClose conn ("Protocolo inválido" :: T.Text)

loopMensagensWorker :: AppState -> WorkerId -> WS.Connection -> IO ()
loopMensagensWorker state wid@(WorkerId widTexto) conn =
  forever (do
    raw <- WS.receiveData conn :: IO BL.ByteString
    case Aeson.decode raw of
      Just (HeartbeatWorker estadoAtual) -> do
        agora <- getCurrentTime
        atualizarHeartbeatWorkerDB (stDb state) widTexto agora
        atomically $ atualizarEstadoWorkerSTM state wid estadoAtual

      Just (AtualizacaoEtapa did etapa) ->
        TIO.putStrLn $ "[WS] " <> widTexto <> " atualizou demanda " <> T.pack (show did) <> " -> " <> T.pack (show etapa)

      Just (DemandaConcluida did saida) ->
        TIO.putStrLn $ "[WS] " <> widTexto <> " atualizou demanda " <> T.pack (show did) <> " em " <> T.pack (show saida)

      Just (DemandaFalhou did etapaErro) ->
        TIO.putStrLn $ "[WS] " <> widTexto <> " falhou na demanda " <> T.pack (show did) <> ": " <> T.pack (show etapaErro)
      _ -> pure ()
  ) `EX.catch` (\(_ :: EX.SomeException) -> pure ())

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  pool  <- inicializarBanco
             ("dados" </> "orquestrador.db")
             ("orchestrator" </> "sql" </> "schema.sql")
  state <- novoAppState pool

  putStrLn "Orquestrador (HTTP + WS) escutando em 0.0.0.0:12000"

  app <- scottyApp $ do
    get "/" $ do
      demandas <- liftIO $ listarDemandasDB (stDb state)
      html $ renderText (paginaDemandas demandas)

    get "/nova-demanda" $
      html $ renderText paginaNovaDemanda

    post "/submeter-demanda" $ do
      nome      <- formParam "usuarioNome"
      matricula <- formParam "usuarioMatricula"
      email     <- formParam "usuarioEmail"
      diretorio <- formParam "demandaDiretorio"
      produto   <- formParam "nomeProdutoFinal"
      perfil    <- formParam "perfilIPED"
      usarPA    <- lerCheckbox "gerarRelatorioPA"
      usarIPED  <- lerCheckbox "executarIndexacaoIPED"

      let solicitante = Usuario
            { usuarioNome      = T.strip nome
            , usuarioMatricula = Matricula (T.strip matricula)
            , usuarioEmail     = Email (T.strip email)
            }
          caminho = CaminhoCompartilhado (T.strip diretorio)
          config  = ConfigProcessamento
            { nomeProdutoFinal      = T.strip produto
            , gerarRelatorioPA      = usarPA
            , executarIndexacaoIPED = usarIPED
            , perfilIPED            = perfil
            , opcoesExtras          = []
            }

      agora <- liftIO getCurrentTime
      _ <- liftIO $ inserirDemandaDB (stDb state) solicitante caminho config agora
      redirect "/"

  -- Warp escuta em todas as interfaces (0.0.0.0) na porta 12000
  Warp.run 12000 $
    websocketsOr WS.defaultConnectionOptions (wsHandler state) app