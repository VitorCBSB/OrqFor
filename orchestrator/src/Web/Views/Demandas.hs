{-# LANGUAGE OverloadedStrings #-}

module Web.Views.Demandas
  ( paginaDemandas
  , fragmentoListaDemandas
  ) where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Time
import Lucid
import Web.Views.Layout

import Database
import Domain.Types

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
        th_ "Diretório"
        th_ "Produto Final"
        th_ "Etapas"
        th_ "Status"
        th_ "Máquina"
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
      td_ $ strong_ ("#" <> toHtml (show (drId d)))
      td_ $ toHtml (drUsuarioNome d <> " (" <> drMatricula d <> ")")
      td_ $ code_ (toHtml (drDiretorio d))
      td_ $ toHtml (drProdutoFinal d)
      td_ $ toHtml (resumoEtapas (drConfig d))
      td_ $ toHtml (drEstadoDesc d)
      td_ $ toHtml (fromMaybe "-" (drWorkerId d))

    resumoEtapas :: ConfigProcessamento -> Text
    resumoEtapas cfg =
      case (gerarRelatorioPA cfg, executarIndexacaoIPED cfg) of
        (True, True)   -> "PA + IPED (" <> perfilIPED cfg <> ")"
        (True, False)  -> "Somente PA"
        (False, True)  -> "Somente IPED (" <> perfilIPED cfg <> ")"
        (False, False) -> "Nenhuma etapa selecionada"
