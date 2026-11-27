{-# LANGUAGE OverloadedStrings #-}

-- | Distribuição das demandas da fila para os workers livres.
module Despachante
  ( loopDespachante
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import qualified Control.Exception as EX
import Control.Monad (forever)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Network.WebSockets as WS

import AppState
import Database (atribuirDemandaWorkerDB, buscarProximaDemandaFilaDB, devolverDemandaParaFilaDB)
import Domain.Protocol (MsgParaWorker (IniciarDemanda))
import Domain.Types
import Log (exibirDemanda, registrar)
import Util (tentar)

-- | Procura demandas NA_FILA e envia para o primeiro worker Livre conectado.
-- Uma falha em qualquer iteração é registrada e o laço continua.
loopDespachante :: AppState -> IO ()
loopDespachante state = forever $ do
  threadDelay 3000000 -- 3 segundos
  resultado <- tentar (despacharProxima state)
  case resultado of
    Left e -> registrar "Despachante" $ "Erro na iteração (laço continua): " <> T.pack (show e)
    Right () -> pure ()

-- | Uma iteração do despachante: reserva um worker, atribui a demanda no banco
-- e a envia. Se algo falhar no meio, desfaz o que já foi feito.
despacharProxima :: AppState -> IO ()
despacharProxima state = do
  mDemanda <- buscarProximaDemandaFilaDB (stDb state)
  case mDemanda of
    Nothing -> pure ()
    Just demanda -> do
      let did = demandaId demanda
      -- Reserva o worker no STM para evitar envio duplo
      mWorkerLivre <- atomically $ do
        mapa <- readTVar (stWorkersAtivos state)
        case filter (\(_, cw) -> cwEstado cw == Livre) (Map.toList mapa) of
          [] -> pure Nothing
          ((wid, cw) : _) -> do
            writeTVar (stWorkersAtivos state)
              (Map.insert wid (cw { cwEstado = Ocupada did, cwConfirmada = False }) mapa)
            pure (Just (wid, cwConn cw))
      case mWorkerLivre of
        Nothing -> pure () -- Nenhum worker livre no momento
        Just (wid@(WorkerId widTexto), conn) -> do
          agora <- getCurrentTime
          -- Passo 1: atribuição no banco. Se falhar, nada foi enviado e o worker
          -- continua utilizável: libera a reserva e propaga o erro.
          atribuido <- tentar (atribuirDemandaWorkerDB (stDb state) did wid agora)
          case atribuido of
            Left e -> do
              atomically $ atualizarEstadoWorkerSTM state wid Livre
              EX.throwIO e
            Right () -> pure ()
          registrar "Despachante" $ "Enviando demanda " <> exibirDemanda did <> " para " <> widTexto
          -- Passo 2: envio. Se falhar, a conexão provavelmente caiu; devolve a
          -- demanda para a fila. O worker permanece reservado até o handler da
          -- conexão removê-lo do mapa (ou até o próximo heartbeat dele).
          enviado <- tentar (WS.sendTextData conn (Aeson.encode (IniciarDemanda demanda)))
          case enviado of
            Left e -> do
              registrar "Despachante" $ "Falha ao enviar demanda " <> exibirDemanda did
                <> " para " <> widTexto <> "; devolvendo à fila: " <> T.pack (show e)
              devolverDemandaParaFilaDB (stDb state) did
            Right () -> pure ()
