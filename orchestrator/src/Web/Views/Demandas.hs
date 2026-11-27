{-# LANGUAGE OverloadedStrings #-}

module Web.Views.Demandas
  ( paginaDemandas
  , fragmentoListaDemandas
  ) where

import Control.Monad (forM_)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, minutesToTimeZone, utcToLocalTime)
import Data.Text (Text, pack)
import Lucid
import Lucid.Base (makeAttribute)
import Web.Views.Layout

import Database
import Domain.Types (Matricula (..), WorkerId (..))
import Filtro
import Log (exibirDemanda)

paginaDemandas :: FiltroDemandas -> (Int, [DemandaResumo]) -> Html ()
paginaDemandas filtro resultado = layoutBase "Orquestrador PA / IPED" $ do
  div_ [style_ "display: flex; justify-content: space-between; align-items: center;"] $ do
    h1_ "Fila de Execução"
    a_ [href_ "/nova-demanda", role_ "button"] "+ Nova Demanda"

  barraFiltros filtro

  -- Atualiza a lista a cada 5 s buscando apenas o fragmento (htmx), sempre com
  -- os valores atuais da barra de filtros
  div_ [ id_ "container-fila"
       , makeAttribute "hx-get" "/fragmento-demandas"
       , makeAttribute "hx-trigger" "every 5s"
       , makeAttribute "hx-include" "#filtros"
       , makeAttribute "hx-swap" "innerHTML"
       ] $
    fragmentoListaDemandas filtro resultado

-- | Barra de filtros. Com htmx, a lista é atualizada enquanto o usuário digita
-- (após 300 ms sem digitar) ou muda a seleção; sem htmx, o formulário funciona
-- como um GET comum para "/".
barraFiltros :: FiltroDemandas -> Html ()
barraFiltros (FiltroDemandas busca situacao somenteUrgentes) =
  form_ [ id_ "filtros"
        , action_ "/"
        , method_ "get"
        , makeAttribute "hx-get" "/fragmento-demandas"
        , makeAttribute "hx-target" "#container-fila"
        , makeAttribute "hx-swap" "innerHTML"
        , makeAttribute "hx-trigger" "input changed delay:300ms from:#busca, change, submit"
        ] $
    div_ [class_ "grid", style_ "align-items: center;"] $ do
      input_ [ type_ "search"
             , id_ "busca"
             , name_ "busca"
             , value_ busca
             , placeholder_ "Buscar por nome, matrícula ou SP (ex.: 3194/2026)"
             , makeAttribute "aria-label" "Buscar"
             ]
      select_ [name_ "situacao", makeAttribute "aria-label" "Situação"] $
        forM_ [minBound .. maxBound] $ \s ->
          option_ ([value_ (codigoSituacao s)] <> [selected_ "" | s == situacao])
            (toHtml (rotuloSituacao s))
      campoCheckbox "Somente urgentes" "urgentes" somenteUrgentes

fragmentoListaDemandas :: FiltroDemandas -> (Int, [DemandaResumo]) -> Html ()
fragmentoListaDemandas filtro (_, [])
  | filtro == filtroVazio = p_ "Nenhuma demanda registrada no banco de dados."
  | otherwise             = p_ "Nenhuma demanda encontrada com esses filtros."
fragmentoListaDemandas _ (total, demandas) = do
  p_ [style_ "margin-bottom: 0.5rem;"] $ small_ $ do
    toHtml (contagem total)
    if total > length demandas
      then toHtml (" (exibindo as " <> pack (show (length demandas)) <> " mais recentes)")
      else pure ()
  table_ $ do
    thead_ $
      tr_ $ do
        th_ "ID"
        th_ "Solicitante"
        th_ "SP"
        th_ "Status"
        th_ "Máquina"
        th_ "Submetida Em"
    tbody_ $
      mapM_ renderLinha demandas
  where
    contagem :: Int -> Text
    contagem 1 = "1 demanda encontrada"
    contagem n = pack (show n) <> " demandas encontradas"

    renderLinha :: DemandaResumo -> Html ()
    renderLinha d = tr_ $ do
      td_ $ do
        strong_ (toHtml (exibirDemanda (drId d)))
        if drUrgente d
          then span_ [style_ "margin-left: 0.5rem; color: #c62828; font-weight: bold"] "[URGENTE]"
          else pure ()
      td_ $ toHtml (drUsuarioNome d <> " (" <> unMatricula (drMatricula d) <> ")")
      td_ $ toHtml ((pack $ show $ drSPNum d) <> "/" <> (pack $ show $ drSPAno d))
      td_ $ toHtml (drEstadoDesc d)
      td_ $ toHtml (maybe "-" unWorkerId (drWorkerId d))
      td_ $ toHtml (formatarHoraBrasilia $ drSubmetidaEm d)

formatarHoraBrasilia :: UTCTime -> Text
formatarHoraBrasilia utc =
  let fusoBrasilia = minutesToTimeZone (-180) -- GMT-3
      local = utcToLocalTime fusoBrasilia utc
  in pack (formatTime defaultTimeLocale "%d/%m/%Y %H:%M" local)
