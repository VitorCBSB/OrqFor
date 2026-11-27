{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Domain.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Identificadores fortemente tipados
newtype DemandaId = DemandaId { unDemandaId :: Int64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype WorkerId = WorkerId { unWorkerId :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype Matricula = Matricula { unMatricula :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype Email = Email { unEmail :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Caminho relativo à raiz do compartilhamento de rede (ex: "2026/Caso_042/Evidencia_01")
newtype CaminhoCompartilhado = CaminhoCompartilhado { unCaminho :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Dados do solicitante
data Usuario = Usuario
  { usuarioNome      :: Text
  , usuarioMatricula :: Matricula
  , usuarioEmail     :: Email
  } deriving stock (Show, Eq, Generic)

instance ToJSON Usuario
instance FromJSON Usuario

data IPEDProfile = 
  IPEDDefault
  | Blind
  | Fastmode
  | Forensic
  | Pedo
  | Triage
  deriving stock (Show, Eq, Generic)

instance ToJSON IPEDProfile
instance FromJSON IPEDProfile

-- | Configurações específicas do processamento (IPED)
data ConfigProcessamento = ConfigProcessamento
  { exportarIPED :: Bool
  , perfilIPED   :: IPEDProfile
  } deriving stock (Show, Eq, Generic)

instance ToJSON ConfigProcessamento
instance FromJSON ConfigProcessamento

-- | Etapas reportadas em tempo real pelo Worker Windows durante a execução de uma
-- demanda ('AtualizacaoEtapa'). O fim da execução não é uma etapa: é informado
-- pelas mensagens 'DemandaConcluida' e 'DemandaFalhou'.
data EtapaProcessamento
  = Inicializando
  | ExecutandoPA
  | ExecutandoIPED
  deriving stock (Show, Eq, Generic)

instance ToJSON EtapaProcessamento
instance FromJSON EtapaProcessamento

-- | Estado operacional da máquina Windows
data EstadoWorker
  = Livre
  | Ocupada DemandaId
  deriving stock (Show, Eq, Generic)

instance ToJSON EstadoWorker
instance FromJSON EstadoWorker

data SolicitacaoPericial = SolicitacaoPericial
  { numero :: Int
  , ano    :: Int
  } deriving stock (Show, Eq, Generic)

instance ToJSON SolicitacaoPericial
instance FromJSON SolicitacaoPericial

-- | Demanda enviada ao Worker para execução
data Demanda = Demanda
  { demandaId          :: DemandaId
  , demandaSolicitante :: Usuario
  , demandaSP          :: SolicitacaoPericial
  , demandaConfig      :: ConfigProcessamento
  , demandaUrgente     :: Bool
  , demandaCriadaEm    :: UTCTime
  } deriving stock (Show, Eq, Generic)

instance ToJSON Demanda
instance FromJSON Demanda