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

-- | Configurações específicas do processamento (PA + IPED)
data ConfigProcessamento = ConfigProcessamento
  { nomeProdutoFinal      :: Text
  , gerarRelatorioPA      :: Bool
  , executarIndexacaoIPED :: Bool
  , perfilIPED            :: Text      -- ex: "default", "fastmode", "forensic"
  , opcoesExtras          :: [Text]    -- Flags adicionais habilitadas na UI
  } deriving stock (Show, Eq, Generic)

instance ToJSON ConfigProcessamento
instance FromJSON ConfigProcessamento

-- | Etapas reportadas em tempo real pelo Worker Windows
data EtapaProcessamento
  = Inicializando
  | ExecutandoPA
  | ExecutandoIPED
  | Finalizado
  | Erro Text
  deriving stock (Show, Eq, Generic)

instance ToJSON EtapaProcessamento
instance FromJSON EtapaProcessamento

-- | Estado operacional da máquina Windows
data EstadoWorker
  = Livre
  | Ocupada DemandaId EtapaProcessamento
  deriving stock (Show, Eq, Generic)

instance ToJSON EstadoWorker
instance FromJSON EstadoWorker

-- | Demanda enviada ao Worker para execução
data Demanda = Demanda
  { demandaId          :: DemandaId
  , demandaSolicitante :: Usuario
  , demandaDiretorio   :: CaminhoCompartilhado
  , demandaConfig      :: ConfigProcessamento
  , demandaUrgente     :: Bool
  , demandaCriadaEm    :: UTCTime
  } deriving stock (Show, Eq, Generic)

instance ToJSON Demanda
instance FromJSON Demanda