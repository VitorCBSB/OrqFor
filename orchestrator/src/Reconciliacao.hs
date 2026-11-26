{-# LANGUAGE OverloadedStrings #-}

-- | Detecção de demandas órfãs: em andamento no banco, mas que nenhum worker
-- está executando nem tem resultado pendente para reenviar.
module Reconciliacao
  ( reconciliarNoHandshake
  , loopVarreduraOrfas
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_, forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)

import AppState
import Database (listarDemandasEmAndamentoDB, marcarDemandaOrfaDB)
import Domain.Types
import Util (tentar)

-- | Prazo sem heartbeat após o qual as demandas em andamento de um worker
-- desconectado são consideradas órfãs. Cobre também o reinício do orquestrador:
-- o worker tem esse tempo para reconectar e informar a demanda que está executando.
prazoDemandaOrfa :: NominalDiffTime
prazoDemandaOrfa = 5 * 60

-- | Compara as demandas que o banco diz estarem em andamento no worker com o que
-- ele informou no handshake. As que ele não está executando e para as quais não
-- tem resultado pendente de reenvio foram perdidas e vão para ERRO.
reconciliarNoHandshake :: AppState -> T.Text -> EstadoWorker -> [DemandaId] -> IO ()
reconciliarNoHandshake state widTexto estadoInformado pendentes = do
  emAndamento <- listarDemandasEmAndamentoDB (stDb state)
  let executando = case estadoInformado of
        Ocupada (DemandaId d) -> Just d
        Livre -> Nothing
      comResultadoPendente did = DemandaId did `elem` pendentes
      orfas = [ did | (did, w, _) <- emAndamento, w == widTexto
                    , Just did /= executando, not (comResultadoPendente did) ]
  agora <- getCurrentTime
  forM_ orfas $ \did -> do
    let motivo = "O worker " <> widTexto <> " reconectou sem esta demanda em execução; o resultado foi perdido."
    marcarDemandaOrfaDB (stDb state) did motivo agora
    TIO.putStrLn $ "[Reconciliação] Demanda #" <> T.pack (show did) <> " marcada como erro: " <> motivo

-- | Periodicamente marca como ERRO as demandas em andamento cujo worker não está
-- conectado e não envia heartbeat há mais de 'prazoDemandaOrfa'.
loopVarreduraOrfas :: AppState -> IO ()
loopVarreduraOrfas state = forever $ do
  threadDelay (60 * 1000000)
  resultado <- tentar $ do
    agora <- getCurrentTime
    emAndamento <- listarDemandasEmAndamentoDB (stDb state)
    conectados <- atomically $ listarWorkersConectadosSTM state
    let semContato hb = maybe True (\t -> diffUTCTime agora t > prazoDemandaOrfa) hb
        orfas = [ (did, w) | (did, w, hb) <- emAndamento
                           , WorkerId w `notElem` conectados
                           , semContato hb ]
    forM_ orfas $ \(did, w) -> do
      let motivo = "O worker " <> w <> " ficou sem contato por mais de "
                   <> T.pack (show (round (prazoDemandaOrfa / 60) :: Int)) <> " min durante o processamento."
      marcarDemandaOrfaDB (stDb state) did motivo agora
      TIO.putStrLn $ "[Varredura] Demanda #" <> T.pack (show did) <> " marcada como erro: " <> motivo
  case resultado of
    Left e -> TIO.putStrLn $ "[Varredura] Erro na iteração (laço continua): " <> T.pack (show e)
    Right () -> pure ()
