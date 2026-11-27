{-# LANGUAGE OverloadedStrings #-}

-- | Estados de uma demanda como gravados no banco (tabela @estados_processamento@).
--
-- Este é o único lugar do código Haskell que conhece os ids numéricos dos
-- estados. Eles precisam coincidir com os inseridos em @sql/schema.sql@; a
-- correspondência é verificada na inicialização do banco ('verificarEstados'
-- em "Database").
module EstadoDemanda
  ( EstadoDemanda (..)
  , idEstado
  , codigoEstado
  , estadoDaEtapa
  , estadosEmExecucao
  , estadosNaoConcluidos
  ) where

import Data.Text (Text)
import Database.SQLite.Simple.ToField (ToField (..))

import Domain.Types (EtapaProcessamento (..))

data EstadoDemanda
  = EstadoNaFila
  | EstadoInicializando
  | EstadoExecutandoPA
  | EstadoExecutandoIPED
  | EstadoFinalizado
  | EstadoErro
  | EstadoCancelado
  deriving (Show, Eq, Enum, Bounded)

-- | Id na tabela @estados_processamento@.
idEstado :: EstadoDemanda -> Int
idEstado EstadoNaFila         = 1
idEstado EstadoInicializando  = 2
idEstado EstadoExecutandoPA   = 3
idEstado EstadoExecutandoIPED = 4
idEstado EstadoFinalizado     = 5
idEstado EstadoErro           = 6
idEstado EstadoCancelado      = 7

-- | Coluna @codigo@ da tabela @estados_processamento@.
codigoEstado :: EstadoDemanda -> Text
codigoEstado EstadoNaFila         = "NA_FILA"
codigoEstado EstadoInicializando  = "INICIALIZANDO"
codigoEstado EstadoExecutandoPA   = "EXECUTANDO_PA"
codigoEstado EstadoExecutandoIPED = "EXECUTANDO_IPED"
codigoEstado EstadoFinalizado     = "FINALIZADO"
codigoEstado EstadoErro           = "ERRO"
codigoEstado EstadoCancelado      = "CANCELADO"

-- | Gravado no banco como o id numérico.
instance ToField EstadoDemanda where
  toField = toField . idEstado

-- | Estado correspondente a uma etapa reportada pelo worker.
estadoDaEtapa :: EtapaProcessamento -> EstadoDemanda
estadoDaEtapa Inicializando  = EstadoInicializando
estadoDaEtapa ExecutandoPA   = EstadoExecutandoPA
estadoDaEtapa ExecutandoIPED = EstadoExecutandoIPED

-- | Demanda atribuída a um worker e ainda não finalizada. É o que a
-- reconciliação considera "em andamento" ao procurar demandas órfãs.
estadosEmExecucao :: [EstadoDemanda]
estadosEmExecucao = [EstadoInicializando, EstadoExecutandoPA, EstadoExecutandoIPED]

-- | Demanda que ainda não terminou: na fila ou em execução. É o que o filtro
-- "Em andamento" da interface Web exibe.
estadosNaoConcluidos :: [EstadoDemanda]
estadosNaoConcluidos = EstadoNaFila : estadosEmExecucao
