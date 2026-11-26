{-# LANGUAGE OverloadedStrings #-}

module Web.Views.Demandas
  ( paginaDemandas
  , fragmentoListaDemandas
  ) where

import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, minutesToTimeZone, utcToLocalTime)
import Data.Text (Text, pack)
import Lucid
import Web.Views.Layout

import Database

paginaDemandas :: [DemandaResumo] -> Html ()
paginaDemandas demandas = layoutBase "Orquestrador PA / IPED" $ do
  div_ [style_ "display: flex; justify-content: space-between; align-items: center;"] $ do
    h1_ "Fila de Execução"
    a_ [href_ "/nova-demanda", role_ "button"] "+ Nova Demanda"

  div_ [id_ "container-fila"] $
    fragmentoListaDemandas demandas

fragmentoListaDemandas :: [DemandaResumo] -> Html ()
fragmentoListaDemandas [] = p_ "Nenhuma demanda registrada no banco de dados."
fragmentoListaDemandas demandas =
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
    renderLinha :: DemandaResumo -> Html ()
    renderLinha d = tr_ $ do
      td_ $ do
        strong_ ("#" <> toHtml (show (drId d)))
        if drUrgente d
          then span_ [style_ "margin-left: 0.5rem; color: #c62828; font-weight: bold"] "[URGENTE]"
          else pure ()
      td_ $ toHtml (drUsuarioNome d <> " (" <> drMatricula d <> ")")
      td_ $ toHtml ((pack $ show $ drSPNum d) <> "/" <> (pack $ show $ drSPAno d))
      td_ $ toHtml (drEstadoDesc d)
      td_ $ toHtml (fromMaybe "-" (drWorkerId d))
      td_ $ toHtml (formatarHoraBrasilia $ drSubmetidaEm d)

formatarHoraBrasilia :: UTCTime -> Text
formatarHoraBrasilia utc =
  let fusoBrasilia = minutesToTimeZone (-180) -- GMT-3
      local = utcToLocalTime fusoBrasilia utc
  in pack (formatTime defaultTimeLocale "%d/%m/%Y %H:%M" local)
