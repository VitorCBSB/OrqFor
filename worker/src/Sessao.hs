{-# LANGUAGE OverloadedStrings #-}

-- | Conexão com o orquestrador: reconexão, handshake, heartbeat e comandos.
module Sessao
  ( manterConexao
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import qualified Control.Exception as EX
import Control.Monad (forM_, forever, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS
import System.Timeout (timeout)

import Ambiente
import Configuracao (caminhoWebSocket, intervaloHeartbeatSegundos, portaOrquestrador)
import Domain.Protocol
import Domain.Types
import Execucao (executarScriptTeste)
import Pendentes (listarPendentes, removerPendente)
import Util (tentar)

-- | Conecta ao orquestrador e reconecta indefinidamente. A espera dobra a cada
-- falha (até 'esperaMaxima') e volta ao valor inicial depois de uma sessão
-- aceita pelo orquestrador.
manterConexao :: String -> Ambiente -> IO ()
manterConexao host amb = loop esperaInicial
  where
    esperaInicial = 2 :: Int
    esperaMaxima = 60 :: Int
    widTexto = unWorkerId (ambWorkerId amb)
    loop espera = do
      TIO.putStrLn $ "Conectando ao orquestrador em " <> T.pack host <> ":" <> T.pack (show portaOrquestrador) <> " como " <> widTexto <> "..."
      r <- tentar (WS.runClient host portaOrquestrador caminhoWebSocket (sessao amb))
      aceito <- case r of
        Right ok -> pure ok
        Left e -> do
          TIO.putStrLn $ "[Conexão] Falha: " <> T.pack (show e)
          pure False
      let espera' = if aceito then esperaInicial else espera
      TIO.putStrLn $ "Reconectando em " <> T.pack (show espera') <> " s..."
      threadDelay (espera' * 1000000)
      loop (min esperaMaxima (espera' * 2))

-- | Uma sessão com o orquestrador: handshake e laço de comandos. Devolve True se o
-- handshake foi aceito (mesmo que a conexão tenha caído depois). Falhas antes do
-- aceite propagam como exceção.
sessao :: Ambiente -> WS.Connection -> IO Bool
sessao amb conn = do
  estadoAtual <- readTVarIO (ambEstado amb)
  pendentes <- listarPendentes (ambDirPendentes amb)
  WS.sendTextData conn (Aeson.encode (HandshakeWorker (ambWorkerId amb) estadoAtual (map fst pendentes)))
  mResposta <- timeout 10000000 (WS.receiveData conn :: IO BL.ByteString)
  case mResposta >>= Aeson.decode of
    Just HandshakeAceito -> do
      TIO.putStrLn "[OK] Handshake aceito pelo orquestrador."
      atomically $ writeTVar (ambConexao amb) (Just conn)
      -- Heartbeat em background reportando o estado real do TVar
      hb <- forkIO $ void $ tentar $ forever $ do
        threadDelay (intervaloHeartbeatSegundos * 1000000)
        est <- readTVarIO (ambEstado amb)
        WS.sendTextData conn (Aeson.encode (HeartbeatWorker est))
      resultado <- tentar (reenviarPendentes pendentes >> comandos conn) `EX.finally` do
        killThread hb
        atomically $ writeTVar (ambConexao amb) Nothing
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
          atomically $ writeTVar (ambEstado amb) (Ocupada (demandaId demanda))
          WS.sendTextData c (Aeson.encode (DemandaAceita (demandaId demanda)))
          -- Roda em thread separada para não bloquear o recebimento de outras mensagens WS
          void $ forkIO $ executarScriptTeste amb demanda
        Just (ResultadoRegistrado did) ->
          removerPendente (ambDirPendentes amb) did
        Just (CancelarDemanda did) ->
          TIO.putStrLn $ "[CMD] Cancelamento solicitado para: " <> T.pack (show did)
        Just SolicitarEstado -> do
          est <- readTVarIO (ambEstado amb)
          WS.sendTextData c (Aeson.encode (HeartbeatWorker est))
        _ ->
          TIO.putStrLn $ "[Aviso] Mensagem inesperada recebida do servidor: " <> T.pack (show cmdRaw)
