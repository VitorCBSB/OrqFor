{-# LANGUAGE RecordWildCards #-}

module AppState
  ( AppState (..)
  , ConexaoWorker (..)
  , novoAppState
  , atualizarEstadoWorkerSTM
  , registrarWorkerSTM
  , removerWorkerSTM
  , listarWorkersConectadosSTM
  ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS

import Database (DBPool)
import Domain.Types (EstadoWorker, WorkerId)

data ConexaoWorker = ConexaoWorker
  { cwConn :: WS.Connection
  , cwEstado :: EstadoWorker
  }

data AppState = AppState
  { stDb :: DBPool
  , stWorkersAtivos :: TVar (Map WorkerId ConexaoWorker)
  }

novoAppState :: DBPool -> IO AppState
novoAppState pool = do
  workers <- newTVarIO Map.empty
  pure (AppState pool workers)

registrarWorkerSTM :: AppState -> WorkerId -> WS.Connection -> EstadoWorker -> STM ()
registrarWorkerSTM st wid conn estado = 
  modifyTVar' (stWorkersAtivos st) (Map.insert wid (ConexaoWorker conn estado))

removerWorkerSTM :: AppState -> WorkerId -> STM ()
removerWorkerSTM st wid = 
  modifyTVar' (stWorkersAtivos st) (Map.delete wid)

atualizarEstadoWorkerSTM :: AppState -> WorkerId -> EstadoWorker -> STM ()
atualizarEstadoWorkerSTM st wid novoEstado =
  modifyTVar' (stWorkersAtivos st) $
    Map.adjust (\cw -> cw { cwEstado = novoEstado }) wid

listarWorkersConectadosSTM :: AppState -> STM [WorkerId]
listarWorkersConectadosSTM st = 
  Map.keys <$> readTVar (stWorkersAtivos st)
