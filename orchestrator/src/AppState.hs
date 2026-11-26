{-# LANGUAGE RecordWildCards #-}

module AppState
  ( AppState (..)
  , ConexaoWorker (..)
  , novoAppState
  , atualizarEstadoWorkerSTM
  , aplicarHeartbeatSTM
  , confirmarDemandaSTM  , registrarWorkerSTM
  , removerWorkerSTM
  , listarWorkersConectadosSTM
  ) where

import Control.Concurrent.STM
import Data.Unique (Unique)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS

import Database (DBPool)
import Domain.Types (DemandaId, EstadoWorker (..), WorkerId)

data ConexaoWorker = ConexaoWorker
  { cwConn :: WS.Connection
  , cwEstado :: EstadoWorker
  , cwToken :: Unique
    -- ^ Identifica esta conexão específica. Permite que o handler de uma conexão
    -- antiga não remova do mapa o registro de uma conexão mais nova do mesmo worker.
  , cwConfirmada :: Bool
    -- ^ False entre o despacho de uma demanda e o 'DemandaAceita' do worker.
    -- Enquanto for False, heartbeats com estado Livre são antigos (enviados
    -- antes de o worker receber a demanda) e devem ser ignorados.
  }

data AppState = AppState
  { stDb :: DBPool
  , stWorkersAtivos :: TVar (Map WorkerId ConexaoWorker)
  }

novoAppState :: DBPool -> IO AppState
novoAppState pool = do
  workers <- newTVarIO Map.empty
  pure (AppState pool workers)

-- | Registra a conexão do worker. Se já havia uma conexão registrada com o mesmo
-- id, ela é substituída e devolvida, para que o chamador possa encerrá-la.
registrarWorkerSTM :: AppState -> WorkerId -> WS.Connection -> EstadoWorker -> Unique -> STM (Maybe WS.Connection)
registrarWorkerSTM st wid conn estado token = do
  mapa <- readTVar (stWorkersAtivos st)
  writeTVar (stWorkersAtivos st) (Map.insert wid (ConexaoWorker conn estado token True) mapa)
  pure (cwConn <$> Map.lookup wid mapa)

-- | Remove o worker do mapa somente se o registro ainda pertencer à conexão
-- identificada por 'token' (ela pode já ter sido substituída por uma nova).
removerWorkerSTM :: AppState -> WorkerId -> Unique -> STM ()
removerWorkerSTM st wid token =
  modifyTVar' (stWorkersAtivos st) $
    Map.update (\cw -> if cwToken cw == token then Nothing else Just cw) wid

-- | Define o estado do worker de forma incondicional (o estado é tratado como confirmado).
atualizarEstadoWorkerSTM :: AppState -> WorkerId -> EstadoWorker -> STM ()
atualizarEstadoWorkerSTM st wid novoEstado =
  modifyTVar' (stWorkersAtivos st) $
    Map.adjust (\cw -> cw { cwEstado = novoEstado, cwConfirmada = True }) wid

-- | Aplica o estado informado em um heartbeat. Se uma demanda foi despachada e
-- ainda não confirmada, um heartbeat Livre é anterior ao recebimento dela
-- pelo worker e é descartado, para não liberar o worker indevidamente.
aplicarHeartbeatSTM :: AppState -> WorkerId -> EstadoWorker -> STM ()
aplicarHeartbeatSTM st wid estadoInformado =
  modifyTVar' (stWorkersAtivos st) $ Map.adjust aplicar wid
  where
    aplicar cw
      | not (cwConfirmada cw), estadoInformado == Livre = cw
      | otherwise = cw { cwEstado = estadoInformado, cwConfirmada = True }

-- | Registra que o worker confirmou o recebimento da demanda despachada.
confirmarDemandaSTM :: AppState -> WorkerId -> DemandaId -> STM ()
confirmarDemandaSTM st wid did =
  modifyTVar' (stWorkersAtivos st) $ Map.adjust confirmar wid
  where
    confirmar cw
      | cwEstado cw == Ocupada did = cw { cwConfirmada = True }
      | otherwise = cw

listarWorkersConectadosSTM :: AppState -> STM [WorkerId]
listarWorkersConectadosSTM st = 
  Map.keys <$> readTVar (stWorkersAtivos st)
