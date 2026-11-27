{-# LANGUAGE OverloadedStrings #-}

-- | Conexões WebSocket dos workers: verificação do caminho, handshake e
-- processamento das mensagens recebidas.
module Ws
  ( wsHandler
  , limiteSilencioSegundos
  ) where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as EX
import Control.Monad (forM_, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (getCurrentTime)
import Data.Unique (newUnique)
import qualified Network.WebSockets as WS
import System.Timeout (timeout)

import AppState
import Configuracao (caminhoWebSocket, intervaloHeartbeatSegundos)
import Database
  ( atualizarEtapaDemandaDB
  , atualizarHeartbeatWorkerDB
  , concluirDemandaDB
  , registrarFalhaDemandaDB
  , validarWorkerDB
  )
import Domain.Protocol
import Domain.Types
import Log (exibirDemanda, exibirEstado, registrar)
import Reconciliacao (reconciliarNoHandshake)
import Util (tentar)

-- | Tempo máximo, em segundos, sem nenhuma mensagem do worker antes de considerar
-- a conexão morta. Três intervalos de heartbeat toleram duas perdas seguidas.
limiteSilencioSegundos :: Int
limiteSilencioSegundos = 3 * intervaloHeartbeatSegundos

-- | Gerencia o ciclo de vida de cada Worker conectado via WebSocket
wsHandler :: AppState -> WS.ServerApp
wsHandler state pending
  | caminho /= BS8.pack caminhoWebSocket = do
      registrar "WS" $ "Conexão WebSocket recusada: caminho " <> TE.decodeUtf8With TEE.lenientDecode caminho
        <> " (esperado " <> T.pack caminhoWebSocket <> ")."
      WS.rejectRequest pending "Caminho WebSocket inválido."
  | otherwise = aceitarWorker state pending
  where
    caminho = WS.requestPath (WS.pendingRequest pending)

-- | Handshake e ciclo de vida de um worker cuja conexão chegou no caminho correto.
aceitarWorker :: AppState -> WS.PendingConnection -> IO ()
aceitarWorker state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    rawMsg <- WS.receiveData conn :: IO BL.ByteString
    case Aeson.eitherDecode rawMsg of
      Right (HandshakeWorker wid@(WorkerId widTexto) estadoInicial pendentes) -> do
        conhecido <- validarWorkerDB (stDb state) wid
        if not conhecido
          then do
            registrar "WS" $ "Conexão recusada para worker desconhecido/inativo: " <> widTexto
            WS.sendTextData conn (Aeson.encode (HandshakeRecusado "Worker não cadastrado ou inativo no orquestrador."))
            WS.sendClose conn ("Recusado" :: T.Text)
          else do
            agora <- getCurrentTime
            atualizarHeartbeatWorkerDB (stDb state) wid agora
            -- Precisa ocorrer ANTES de registrar o worker no STM: depois do registro
            -- o despachante pode atribuir uma demanda nova a ele, que seria
            -- confundida com uma órfã.
            reconciliarNoHandshake state wid estadoInicial pendentes
            token <- newUnique
            anterior <- atomically $ registrarWorkerSTM state wid conn estadoInicial token
            -- Reconexão do mesmo worker: encerra a conexão antiga para que o handler
            -- dela não continue alterando o estado do registro novo.
            forM_ anterior $ \antiga -> do
              registrar "WS" $ "Worker " <> widTexto <> " reconectou; encerrando conexão anterior."
              void $ tentar (WS.sendClose antiga ("Substituida por nova conexao" :: T.Text))
            registrar "WS" $ "Worker conectado e autenticado: " <> widTexto <> " | Estado: " <> exibirEstado estadoInicial
            WS.sendTextData conn (Aeson.encode HandshakeAceito)

            -- Mantém a escuta de heartbeats e garante remoção do STM ao desconectar
            loopMensagensWorker state wid conn
              `EX.finally` do
                atomically $ removerWorkerSTM state wid token
                registrar "WS" $ "Worker desconectado: " <> widTexto

      outra -> do
        let detalhe = case outra of
              Left erro -> "não decodificou (" <> T.pack erro <> ")"
              Right _ -> "a primeira mensagem não é um handshake"
        registrar "WS" $ "Handshake inválido: " <> detalhe <> ". Início da mensagem: " <> trechoMensagem rawMsg
        WS.sendTextData conn (Aeson.encode (HandshakeRecusado "Handshake inválido."))
        WS.sendClose conn ("Protocolo inválido" :: T.Text)

-- | Recebe e processa mensagens do worker até a conexão cair ou o worker ficar
-- em silêncio por mais de 'limiteSilencioSegundos' (conexão meio aberta).
loopMensagensWorker :: AppState -> WorkerId -> WS.Connection -> IO ()
loopMensagensWorker state wid@(WorkerId widTexto) conn = do
  -- Falhas de conexão (fechamento, erro de socket) encerram o laço e são
  -- registradas. Exceções assíncronas (ex.: ThreadKilled) são repropagadas.
  resultado <- tentar laco
  case resultado of
    Left e -> registrar "WS" $ "Conexão com " <> widTexto <> " encerrada: " <> T.pack (show e)
    Right () -> pure ()
  where
    laco = do
      mRaw <- timeout (limiteSilencioSegundos * 1000000) (WS.receiveData conn :: IO BL.ByteString)
      case mRaw of
        Nothing ->
          registrar "WS" $ widTexto <> " sem mensagens há "
            <> T.pack (show limiteSilencioSegundos) <> " s; encerrando a conexão."
        Just rawData -> do
          -- Uma falha ao processar uma mensagem (ex.: erro no banco) é registrada
          -- sem derrubar a conexão.
          r <- tentar (processar rawData)
          case r of
            Left e -> registrar "WS" $ "Erro ao processar mensagem de " <> widTexto
                        <> " (conexão mantida): " <> T.pack (show e)
            Right () -> pure ()
          laco

    processar rawData =
      case Aeson.eitherDecode rawData of
        Left erro ->
          registrar "WS" $ "Mensagem não reconhecida de " <> widTexto <> " (" <> T.pack erro
            <> "). Worker e orquestrador estão na mesma versão do protocolo? Início da mensagem: "
            <> trechoMensagem rawData
        Right HandshakeWorker{} ->
          registrar "WS" $ widTexto <> " enviou um handshake repetido; ignorado."
        Right (HeartbeatWorker estadoAtual) -> do
          agora <- getCurrentTime
          atualizarHeartbeatWorkerDB (stDb state) wid agora
          atomically $ aplicarHeartbeatSTM state wid estadoAtual

        Right (DemandaAceita did) ->
          atomically $ confirmarDemandaSTM state wid did
        Right (AtualizacaoEtapa did etapa) -> do
          atualizarEtapaDemandaDB (stDb state) did etapa
          registrar "WS" $ widTexto <> " atualizou demanda " <> exibirDemanda did <> " -> " <> T.pack (show etapa)

        Right (DemandaConcluida did saida) -> do
          agora <- getCurrentTime
          concluirDemandaDB (stDb state) did saida agora
          atomically $ liberarWorkerSTM state wid did
          registrar "WS" $ widTexto <> " concluiu demanda " <> exibirDemanda did <> " em " <> unCaminho saida
          confirmarResultado did

        Right (DemandaFalhou did motivo) -> do
          agora <- getCurrentTime
          registrarFalhaDemandaDB (stDb state) did motivo agora
          atomically $ liberarWorkerSTM state wid did
          registrar "WS" $ widTexto <> " falhou na demanda " <> exibirDemanda did <> ": " <> motivo
          confirmarResultado did

    -- Avisa o worker que o resultado foi gravado, para ele descartar a cópia
    -- local. Só é chamado depois da gravação no banco: se ela falhar, a exceção
    -- impede a confirmação e o worker reenvia na próxima conexão. Uma falha no
    -- envio da confirmação também só causa um reenvio (a gravação é idempotente).
    confirmarResultado did = do
      r <- tentar (WS.sendTextData conn (Aeson.encode (ResultadoRegistrado did)))
      case r of
        Left e -> registrar "WS" $ "Falha ao confirmar resultado a " <> widTexto <> ": " <> T.pack (show e)
        Right () -> pure ()

-- | Primeiros 200 bytes de uma mensagem, para log (sem despejar mensagens enormes).
trechoMensagem :: BL.ByteString -> T.Text
trechoMensagem msg =
  TE.decodeUtf8With TEE.lenientDecode (BL.toStrict (BL.take 200 msg))
    <> (if BL.length msg > 200 then "..." else "")
