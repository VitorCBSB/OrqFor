-- | Estado compartilhado do orquestrador: o banco e os workers conectados.
-- As regras de transição da situação de cada worker ficam em "SituacaoWorker";
-- aqui elas são aplicadas sobre o mapa de workers, em STM.
module AppState
  ( AppState (..)
  , ConexaoWorker (..)
  , novoAppState
  , registrarWorkerSTM
  , removerWorkerSTM
  , reservarWorkerLivreSTM
  , cancelarDespachoSTM
  , aplicarHeartbeatSTM
  , confirmarDemandaSTM
  , liberarWorkerSTM
  , listarWorkersConectadosSTM
  ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Unique (Unique)
import qualified Network.WebSockets as WS

import Database (DBPool)
import Domain.Types (DemandaId, EstadoWorker, WorkerId)
import SituacaoWorker

data ConexaoWorker = ConexaoWorker
  { cwConn :: WS.Connection
  , cwToken :: Unique
    -- ^ Identifica esta conexão específica. Permite que o handler de uma conexão
    -- antiga não remova do mapa o registro de uma conexão mais nova do mesmo worker.
  , cwSituacao :: SituacaoWorker
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
  writeTVar (stWorkersAtivos st) (Map.insert wid (ConexaoWorker conn token (situacaoInicial estado)) mapa)
  pure (cwConn <$> Map.lookup wid mapa)

-- | Remove o worker do mapa somente se o registro ainda pertencer à conexão
-- identificada por 'token' (ela pode já ter sido substituída por uma nova).
removerWorkerSTM :: AppState -> WorkerId -> Unique -> STM ()
removerWorkerSTM st wid token =
  modifyTVar' (stWorkersAtivos st) $
    Map.update (\cw -> if cwToken cw == token then Nothing else Just cw) wid

-- | Escolhe um worker livre (o de menor id), reserva-o para a demanda e devolve
-- a conexão dele. A escolha e a reserva acontecem na mesma transação, para que
-- o mesmo worker não receba duas demandas.
reservarWorkerLivreSTM :: AppState -> DemandaId -> STM (Maybe (WorkerId, WS.Connection))
reservarWorkerLivreSTM st did = do
  mapa <- readTVar (stWorkersAtivos st)
  case Map.lookupMin (Map.filter (estaLivre . cwSituacao) mapa) of
    Nothing -> pure Nothing
    Just (wid, cw) -> do
      writeTVar (stWorkersAtivos st)
        (Map.insert wid cw { cwSituacao = aoDespachar did (cwSituacao cw) } mapa)
      pure (Just (wid, cwConn cw))

cancelarDespachoSTM :: AppState -> WorkerId -> STM ()
cancelarDespachoSTM st wid = ajustarSituacao st wid aoCancelarDespacho

aplicarHeartbeatSTM :: AppState -> WorkerId -> EstadoWorker -> STM ()
aplicarHeartbeatSTM st wid estado = ajustarSituacao st wid (aoReceberHeartbeat estado)

confirmarDemandaSTM :: AppState -> WorkerId -> DemandaId -> STM ()
confirmarDemandaSTM st wid did = ajustarSituacao st wid (aoConfirmarDemanda did)

liberarWorkerSTM :: AppState -> WorkerId -> DemandaId -> STM ()
liberarWorkerSTM st wid did = ajustarSituacao st wid (aoReceberResultado did)

listarWorkersConectadosSTM :: AppState -> STM [WorkerId]
listarWorkersConectadosSTM st =
  Map.keys <$> readTVar (stWorkersAtivos st)

-- | Aplica uma transição à situação de um worker, se ele estiver conectado.
ajustarSituacao :: AppState -> WorkerId -> (SituacaoWorker -> SituacaoWorker) -> STM ()
ajustarSituacao st wid f =
  modifyTVar' (stWorkersAtivos st) $
    Map.adjust (\cw -> cw { cwSituacao = f (cwSituacao cw) }) wid
