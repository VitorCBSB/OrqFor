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
  deriving stock (Show, Eq, Generic)

instance ToJSON MsgDoWorker
instance FromJSON MsgDoWorker