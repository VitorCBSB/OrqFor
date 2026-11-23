{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Domain.Protocol
import Domain.Types
import qualified Network.WebSockets as WS
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stderr, stdout, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  args <- getArgs
  (host, widTexto) <- case args of
    [h, w] -> pure (h, T.pack w)
    _ -> do
      putStrLn "Uso: worker.exe <IP_ORQUESTRADOR> <WORKER_ID>"
      exitFailure

  let wid = WorkerId widTexto
      -- Ajuste o construtor abaixo conforme o nome exato em Domain.Types (ex: Ocioso ou Livre)
      estadoInicial = Livre

  TIO.putStrLn $ "Conectando ao orquestrador em " <> T.pack host <> ":12000 como " <> widTexto <> "..."

  WS.runClient host 12000 "/ws" $ \conn -> do
    -- 1. Envia o HandshakeWorker definido no Protocol.hs
    WS.sendTextData conn (Aeson.encode (HandshakeWorker wid estadoInicial))

    -- 2. Aguarda resposta do orquestrador
    respostaRaw <- WS.receiveData conn :: IO BL.ByteString
    case Aeson.decode respostaRaw of
      Just HandshakeAceito -> do
        TIO.putStrLn "[OK] Handshake aceito pelo orquestrador."

        -- Thread em background enviando heartbeat a cada 15s
        void $ forkIO $ forever $ do
          threadDelay 15000000
          WS.sendTextData conn (Aeson.encode (HeartbeatWorker estadoInicial))

        -- Loop principal aguardando comandos do orquestrador (MsgParaWorker)
        forever $ do
          cmdRaw <- WS.receiveData conn :: IO BL.ByteString
          case Aeson.decode cmdRaw of
            Just (IniciarDemanda demanda) ->
              TIO.putStrLn $ "[CMD] Recebida ordem para iniciar demanda: " <> T.pack (show (demandaId demanda))
            Just (CancelarDemanda did) ->
              TIO.putStrLn $ "[CMD] Recebida ordem para cancelar demanda: " <> T.pack (show did)
            Just SolicitarEstado ->
              WS.sendTextData conn (Aeson.encode (HeartbeatWorker estadoInicial))
            _ ->
              TIO.putStrLn "[Aviso] Mensagem inesperada recebida do servidor."

      Just (HandshakeRecusado motivo) -> do
        TIO.putStrLn $ "[ERRO] Conexão recusada: " <> motivo
        WS.sendClose conn ("Saindo" :: T.Text)

      _ -> do
        TIO.putStrLn "[ERRO] Resposta inválida no handshake."
        WS.sendClose conn ("Saindo" :: T.Text)