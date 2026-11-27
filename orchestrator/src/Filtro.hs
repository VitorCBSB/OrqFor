{-# LANGUAGE OverloadedStrings #-}

-- | Filtros da lista de demandas na interface Web.
module Filtro
  ( FiltroDemandas (..)
  , Situacao (..)
  , filtroVazio
  , codigoSituacao
  , rotuloSituacao
  , situacaoDoCodigo
  , parametrosDoFiltro
  ) where

import Data.Text (Text)

-- | Agrupamento dos estados da demanda oferecido ao perito.
data Situacao
  = Todas
  | EmAndamento  -- ^ na fila, inicializando ou executando PA/IPED
  | Enfileiradas
  | Concluidas
  | ComErro
  deriving (Show, Eq, Enum, Bounded)

data FiltroDemandas = FiltroDemandas
  { filtroBusca           :: Text  -- ^ nome, matrícula ou SP; vazio = sem filtro
  , filtroSituacao        :: Situacao
  , filtroSomenteUrgentes :: Bool
  } deriving (Show, Eq)

filtroVazio :: FiltroDemandas
filtroVazio = FiltroDemandas "" Todas False

-- | Valor usado na URL (ex.: @?situacao=erro@).
codigoSituacao :: Situacao -> Text
codigoSituacao Todas       = "todas"
codigoSituacao EmAndamento = "andamento"
codigoSituacao Enfileiradas = "fila"
codigoSituacao Concluidas  = "concluidas"
codigoSituacao ComErro     = "erro"

-- | Texto exibido na lista de seleção.
rotuloSituacao :: Situacao -> Text
rotuloSituacao Todas       = "Todas"
rotuloSituacao EmAndamento = "Em andamento"
rotuloSituacao Enfileiradas = "Na fila"
rotuloSituacao Concluidas  = "Concluídas"
rotuloSituacao ComErro     = "Com erro"

-- | Código desconhecido vira 'Todas' (o valor vem da URL, que o usuário pode editar).
situacaoDoCodigo :: Text -> Situacao
situacaoDoCodigo codigo =
  case [ s | s <- [minBound .. maxBound], codigoSituacao s == codigo ] of
    (s : _) -> s
    []      -> Todas

-- | Parâmetros de URL que representam o filtro, omitindo os valores padrão.
parametrosDoFiltro :: FiltroDemandas -> [(Text, Text)]
parametrosDoFiltro (FiltroDemandas busca situacao urgentes) =
  [ ("busca", busca) | busca /= "" ]
    <> [ ("situacao", codigoSituacao situacao) | situacao /= Todas ]
    <> [ ("urgentes", "1") | urgentes ]
