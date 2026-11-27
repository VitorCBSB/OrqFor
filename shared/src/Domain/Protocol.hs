{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Domain.Protocol where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Domain.Types
import GHC.Generics (Generic)

-- | Mensagens enviadas: Orquestrador (Linux) -> Worker (Windows)
data MsgParaWorker
  = IniciarDemanda Demanda
  | CancelarDemanda DemandaId
  | SolicitarEstado
  | HandshakeAceito 
  | HandshakeRecusado Text
  | ResultadoRegistrado DemandaId
    -- ^ O resultado final ('DemandaConcluida'/'DemandaFalhou') foi gravado no
    -- banco; o worker pode descartar a cópia guardada para reenvio.
  deriving stock (Show, Eq, Generic)

instance ToJSON MsgParaWorker
instance FromJSON MsgParaWorker

-- | Mensagens enviadas: Worker (Windows) -> Orquestrador (Linux)
data MsgDoWorker
  = HandshakeWorker
      { hwWorkerId    :: WorkerId
      , hwEstadoAtual :: EstadoWorker  -- Crucial para recuperação após queda de rede
      , hwResultadosPendentes :: [DemandaId]
        -- ^ Demandas cujo resultado final ainda não foi confirmado pelo
        -- orquestrador; o worker os reenvia logo após o handshake.
      }
  | HeartbeatWorker EstadoWorker
  | DemandaAceita   -- Confirma o recebimento de 'IniciarDemanda'; o worker já está Ocupada
      { daDemandaId :: DemandaId
      }
  | AtualizacaoEtapa
      { aeDemandaId :: DemandaId
      , aeNovaEtapa :: EtapaProcessamento
      }
  | DemandaConcluida
      { dcDemandaId     :: DemandaId
      , dcCaminhoSaida  :: CaminhoCompartilhado
      }
  | DemandaFalhou
      { dfDemandaId :: DemandaId
      , dfMotivo    :: Text -- Ex: "Timeout no IPED". A demanda sempre vai para ERRO.
      }
  | DemandaRecusada
      -- ^ Resposta a 'IniciarDemanda' quando o worker não pode executar a demanda
      -- (ex.: a sessão gráfica ficou inutilizável). Nada foi executado; a demanda
      -- volta para a fila.
      { rcDemandaId :: DemandaId
      , rcMotivo    :: Text
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON MsgDoWorker
instance FromJSON MsgDoWorker