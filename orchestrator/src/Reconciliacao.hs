{-# LANGUAGE OverloadedStrings #-}

-- | Detecção de demandas órfãs: em execução no banco, mas que nenhum worker
-- está executando nem tem resultado pendente para reenviar.
module Reconciliacao
  ( reconciliarNoHandshake
  , loopVarreduraOrfas
  , prazoDemandaOrfa
  , orfasNoHandshake
  , orfasSemContato
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_, forever)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import AppState
import Database (DemandaEmExecucao (..), listarDemandasEmExecucaoDB, marcarDemandaOrfaDB)
import Domain.Types
import Log (exibirDemanda, registrar)
import Util (tentar)

-- | Prazo sem heartbeat após o qual as demandas em execução de um worker
-- desconectado são consideradas órfãs. Cobre também o reinício do orquestrador:
-- o worker tem esse tempo para reconectar e informar a demanda que está executando.
prazoDemandaOrfa :: NominalDiffTime
prazoDemandaOrfa = 5 * 60

-- | Demandas que o banco diz estarem em execução no worker, mas que ele, no
-- handshake, não informou estar executando nem tem resultado pendente de
-- reenvio. Essas foram perdidas.
orfasNoHandshake :: WorkerId -> EstadoWorker -> [DemandaId] -> [DemandaEmExecucao] -> [DemandaId]
orfasNoHandshake wid estadoInformado pendentes emExecucao =
  [ did
  | d <- emExecucao
  , deWorker d == wid
  , let did = deDemanda d
  , Just did /= executando
  , did `notElem` pendentes
  ]
  where
    -- Livre e Indisponivel: o worker não está executando nada
    executando = case estadoInformado of
      Ocupada d -> Just d
      Livre -> Nothing
      Indisponivel _ -> Nothing

-- | Demandas em execução cujo worker não está conectado e não envia heartbeat
-- há mais de 'prazoDemandaOrfa' (ou nunca enviou).
orfasSemContato :: UTCTime -> [WorkerId] -> [DemandaEmExecucao] -> [DemandaEmExecucao]
orfasSemContato agora conectados = filter $ \d ->
  deWorker d `notElem` conectados && semContato (deUltimoHeartbeat d)
  where
    semContato = maybe True (\t -> diffUTCTime agora t > prazoDemandaOrfa)

-- | Marca como ERRO as demandas que o worker perdeu (ver 'orfasNoHandshake').
reconciliarNoHandshake :: AppState -> WorkerId -> EstadoWorker -> [DemandaId] -> IO ()
reconciliarNoHandshake state wid estadoInformado pendentes = do
  emExecucao <- listarDemandasEmExecucaoDB (stDb state)
  agora <- getCurrentTime
  forM_ (orfasNoHandshake wid estadoInformado pendentes emExecucao) $ \did -> do
    let motivo = "O worker " <> unWorkerId wid <> " reconectou sem esta demanda em execução; o resultado foi perdido."
    marcarDemandaOrfaDB (stDb state) did motivo agora
    registrar "Reconciliação" $ "Demanda " <> exibirDemanda did <> " marcada como erro: " <> motivo

-- | Periodicamente marca como ERRO as demandas de workers que sumiram
-- (ver 'orfasSemContato').
loopVarreduraOrfas :: AppState -> IO ()
loopVarreduraOrfas state = forever $ do
  threadDelay (60 * 1000000)
  resultado <- tentar $ do
    agora <- getCurrentTime
    emExecucao <- listarDemandasEmExecucaoDB (stDb state)
    conectados <- atomically $ listarWorkersConectadosSTM state
    forM_ (orfasSemContato agora conectados emExecucao) $ \d -> do
      let motivo = "O worker " <> unWorkerId (deWorker d) <> " ficou sem contato por mais de "
                   <> T.pack (show (round (prazoDemandaOrfa / 60) :: Int)) <> " min durante o processamento."
      marcarDemandaOrfaDB (stDb state) (deDemanda d) motivo agora
      registrar "Varredura" $ "Demanda " <> exibirDemanda (deDemanda d) <> " marcada como erro: " <> motivo
  case resultado of
    Left e -> registrar "Varredura" $ "Erro na iteração (laço continua): " <> T.pack (show e)
    Right () -> pure ()
