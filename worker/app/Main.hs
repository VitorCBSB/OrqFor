{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (forever, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Domain.Protocol
import Domain.Types
import qualified Network.WebSockets as WS
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import System.Process (proc, readCreateProcessWithExitCode)

-- | Executa o script AutoHotkey de teste e reporta o resultado ao Orquestrador
executarScriptTeste :: WS.Connection -> TVar EstadoWorker -> Demanda -> IO ()
executarScriptTeste conn varEstado demanda = do
  let did = demandaId demanda
      caminhoAHK = "C:\\Users\\PA\\Desktop\\PA_Auto\\test.ahk"
      -- Se o AutoHotkey.exe não estiver no PATH do Windows, use o caminho completo:
      -- ex: "C:\\Program Files\\AutoHotkey\\v2\\AutoHotkey64.exe"
      executavelAHK = "C:\\Users\\PA\\Desktop\\PA_Auto\\deps\\AutoHotKey\\AutoHotkey64.exe"

  TIO.putStrLn $ "[Execução] Disparando AutoHotkey: " <> T.pack caminhoAHK

  -- Executa de forma síncrona (aguarda o script terminar)
  (exitCode, _stdout, stderrStr) <-
    readCreateProcessWithExitCode (proc executavelAHK [caminhoAHK]) ""

  case exitCode of
    ExitSuccess -> do
      TIO.putStrLn "[Execução] Script AutoHotkey finalizado com sucesso."
      let saidaTeste = CaminhoCompartilhado "C:\\Users\\PA\\Desktop\\PA_Auto\\saida_teste"
      WS.sendTextData conn (Aeson.encode (DemandaConcluida did saidaTeste))

    ExitFailure codigo -> do
      let motivo = "AutoHotkey retornou erro (código " <> T.pack (show codigo) <> "): " <> T.pack stderrStr
      TIO.putStrLn $ "[Execução] " <> motivo
      WS.sendTextData conn (Aeson.encode (DemandaFalhou did (Erro motivo)))

  atomically $ writeTVar varEstado Livre

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
  varEstado <- newTVarIO Livre

  TIO.putStrLn $ "Conectando ao orquestrador em " <> T.pack host <> ":12000 como " <> widTexto <> "..."

  WS.runClient host 12000 "/ws" $ \conn -> do
    estadoAtual <- readTVarIO varEstado
    WS.sendTextData conn (Aeson.encode (HandshakeWorker wid estadoAtual))

    respostaRaw <- WS.receiveData conn :: IO BL.ByteString
    case Aeson.decode respostaRaw of
      Just HandshakeAceito -> do
        TIO.putStrLn "[OK] Handshake aceito pelo orquestrador."

        -- Heartbeat em background reportando o estado real do TVar
        void $ forkIO $ forever $ do
          threadDelay 15000000
          est <- readTVarIO varEstado
          WS.sendTextData conn (Aeson.encode (HeartbeatWorker est))

        -- Loop de comandos vindos do orquestrador
        forever $ do
          cmdRaw <- WS.receiveData conn :: IO BL.ByteString
          case Aeson.decode cmdRaw of
            Just (IniciarDemanda demanda) -> do
              TIO.putStrLn $ "[CMD] Recebida demanda: " <> T.pack (show (demandaId demanda))
              -- Marca Ocupada ANTES de criar a thread e confirma o recebimento ao
              -- orquestrador, para que heartbeats seguintes nunca informem Livre
              -- com uma demanda em mãos.
              atomically $ writeTVar varEstado (Ocupada (demandaId demanda))
              WS.sendTextData conn (Aeson.encode (DemandaAceita (demandaId demanda)))
              -- Roda em thread separada para não bloquear o recebimento de outras mensagens WS
              void $ forkIO $ executarScriptTeste conn varEstado demanda

            Just (CancelarDemanda did) ->
              TIO.putStrLn $ "[CMD] Cancelamento solicitado para: " <> T.pack (show did)

            Just SolicitarEstado -> do
              est <- readTVarIO varEstado
              WS.sendTextData conn (Aeson.encode (HeartbeatWorker est))

            _ ->
              TIO.putStrLn $ "[Aviso] Mensagem inesperada recebida do servidor: " <> (T.pack $ show cmdRaw)

      Just (HandshakeRecusado motivo) -> do
        TIO.putStrLn $ "[ERRO] Conexão recusada: " <> motivo
        WS.sendClose conn ("Saindo" :: T.Text)

      _ -> do
        TIO.putStrLn "[ERRO] Resposta inválida no handshake."
        WS.sendClose conn ("Saindo" :: T.Text)