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
  deriving stock (Show, Eq, Generic)

instance ToJSON MsgParaWorker
instance FromJSON MsgParaWorker

-- | Mensagens enviadas: Worker (Windows) -> Orquestrador (Linux)
data MsgDoWorker
  = HandshakeWorker
      { hwWorkerId    :: WorkerId
      , hwEstadoAtual :: EstadoWorker  -- Crucial para recuperação após queda de rede
      }
  | HeartbeatWorker EstadoWorker
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
      , dfEtapa     :: EtapaProcessamento -- Ex: Erro "Timeout no IPED"
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON MsgDoWorker
instance FromJSON MsgDoWorker