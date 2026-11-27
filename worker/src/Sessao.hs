{-# LANGUAGE OverloadedStrings #-}

-- | Conexão com o orquestrador: reconexão, handshake, heartbeat e comandos.
module Sessao
  ( manterConexao
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, writeTVar)
import qualified Control.Exception as EX
import Control.Monad (forM_, forever, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Timeout (timeout)

import Ambiente
import ConfigWorker (ConfigWorker (..))
import Configuracao (caminhoWebSocket, intervaloHeartbeatSegundos, portaOrquestrador)
import Domain.Protocol
import Domain.Types
import Execucao (executarScriptTeste)
import Log (exibirDemanda, registrar)
import Pendentes (listarPendentes, removerPendente)
import SessaoGrafica (SituacaoSessao (..))
import Util (tentar)

-- | Conecta ao orquestrador e reconecta indefinidamente. A espera dobra a cada
-- falha (até 'esperaMaxima') e volta ao valor inicial depois de uma sessão
-- aceita pelo orquestrador.
manterConexao :: Ambiente -> IO ()
manterConexao amb = loop esperaInicial
  where
    esperaInicial = 2 :: Int
    esperaMaxima = 60 :: Int
    widTexto = unWorkerId (ambWorkerId amb)
    host = cfgOrquestrador (ambConfig amb)
    loop espera = do
      registrar "Conexão" $ "Conectando ao orquestrador em " <> T.pack host <> ":" <> T.pack (show portaOrquestrador) <> " como " <> widTexto <> "..."
      r <- tentar (WS.runClient host portaOrquestrador caminhoWebSocket (sessao amb))
      aceito <- case r of
        Right ok -> pure ok
        Left e -> do
          registrar "Conexão" $ "Falha: " <> T.pack (show e)
          pure False
      let espera' = if aceito then esperaInicial else espera
      registrar "Conexão" $ "Reconectando em " <> T.pack (show espera') <> " s..."
      threadDelay (espera' * 1000000)
      loop (min esperaMaxima (espera' * 2))

-- | Uma sessão com o orquestrador: handshake e laço de comandos. Devolve True se o
-- handshake foi aceito (mesmo que a conexão tenha caído depois). Falhas antes do
-- aceite propagam como exceção.
sessao :: Ambiente -> WS.Connection -> IO Bool
sessao amb conn = do
  estadoAtual <- estadoParaInformar amb
  pendentes <- listarPendentes (ambDirPendentes amb)
  WS.sendTextData conn (Aeson.encode (HandshakeWorker (ambWorkerId amb) estadoAtual (map fst pendentes)))
  mResposta <- timeout 10000000 (WS.receiveData conn :: IO BL.ByteString)
  case mResposta >>= Aeson.decode of
    Just HandshakeAceito -> do
      registrar "Conexão" "Handshake aceito pelo orquestrador."
      atomically $ writeTVar (ambConexao amb) (Just conn)
      -- Heartbeat em background. O estado é decidido e enviado dentro da trava
      -- (ver 'comTravaDeEstado').
      hb <- forkIO $ void $ tentar $ forever $ do
        threadDelay (intervaloHeartbeatSegundos * 1000000)
        informarEstado conn
      resultado <- tentar (reenviarPendentes pendentes >> comandos conn) `EX.finally` do
        killThread hb
        atomically $ writeTVar (ambConexao amb) Nothing
      case resultado of
        Left e -> registrar "Conexão" $ "Encerrada: " <> T.pack (show e)
        Right () -> registrar "Conexão" "Encerrada."
      pure True
    Just (HandshakeRecusado motivo) -> do
      registrar "Conexão" $ "Recusada pelo orquestrador: " <> motivo
      WS.sendClose conn ("Saindo" :: T.Text)
      pure False
    _ -> do
      registrar "Conexão" "Resposta inválida ou ausente no handshake."
      WS.sendClose conn ("Saindo" :: T.Text)
      pure False
  where
    informarEstado c = comTravaDeEstado amb $ do
      est <- estadoParaInformar amb
      WS.sendTextData c (Aeson.encode (HeartbeatWorker est))

    reenviarPendentes pendentes =
      forM_ pendentes $ \(did, msg) -> do
        registrar "Pendentes" $ "Reenviando resultado da demanda " <> exibirDemanda did
        WS.sendTextData conn (Aeson.encode msg)

    -- Loop de comandos vindos do orquestrador; só termina por exceção
    comandos c = forever $ do
      cmdRaw <- WS.receiveData c :: IO BL.ByteString
      case Aeson.decode cmdRaw of
        Just (IniciarDemanda demanda) -> do
          let did = demandaId demanda
          registrar "Comando" $ "Recebida demanda " <> exibirDemanda did
          -- A sessão pode ter ficado inutilizável depois do último heartbeat.
          -- A decisão e a marcação de Ocupada ficam dentro da trava, para que
          -- nenhum heartbeat com estado anterior seja enviado depois da resposta.
          situacao <- comTravaDeEstado amb $ do
            s <- atualizarSituacaoSessao amb
            case s of
              SessaoUtilizavel -> atomically $ writeTVar (ambEstado amb) (Ocupada did)
              SessaoInutilizavel _ -> pure ()
            pure s
          case situacao of
            SessaoUtilizavel -> do
              -- A execução começa antes da confirmação: se o envio falhar (conexão
              -- caindo), a demanda continua sendo executada e o resultado é
              -- reenviado depois (ver módulo "Pendentes").
              void $ forkIO $ executarScriptTeste amb demanda
              WS.sendTextData c (Aeson.encode (DemandaAceita did))
            SessaoInutilizavel motivo -> do
              registrar "Comando" $ "Demanda " <> exibirDemanda did <> " recusada: " <> motivo
              WS.sendTextData c (Aeson.encode (DemandaRecusada did motivo))
        Just (ResultadoRegistrado did) ->
          removerPendente (ambDirPendentes amb) did
        Just (CancelarDemanda did) ->
          registrar "Comando" $ "Cancelamento solicitado para a demanda " <> exibirDemanda did
        Just SolicitarEstado ->
          informarEstado c
        _ ->
          registrar "Comando" $ "Mensagem inesperada recebida do orquestrador: " <> T.pack (show cmdRaw)
