{-# LANGUAGE OverloadedStrings #-}

module Web.Views.NovaDemanda (
  paginaNovaDemanda,
  formularioDemanda
) where

import Web.Views.Layout
import Lucid
import Lucid.Base (makeAttribute)

paginaNovaDemanda :: Html ()
paginaNovaDemanda = layoutBase "Nova Demanda" $ do
  h1_ "Nova Demanda de Processamento"
  formularioDemanda

formularioDemanda :: Html ()
formularioDemanda =
  form_ [ action_ "/submeter-demanda", method_ "post" ] $ do
      fieldset_ $ do
        legend_ (strong_ "Dados do Solicitante")
        div_ [class_ "grid"] $ do
          campoTexto "Nome completo" "usuarioNome" "Ex: Maria Silva" True
          campoTexto "Matrícula" "usuarioMatricula" "Ex: 987654" True
        label_ [for_ "usuarioEmail"] $ do
          "E-mail para notificação"
          input_
            [ type_ "email"
            , id_ "usuarioEmail"
            , name_ "usuarioEmail"
            , placeholder_ "maria.silva@org.gov.br"
            , makeAttribute "required" ""
            ]

      fieldset_ $ do
        legend_ (strong_ "Diretório e Configurações")
        campoTexto
          "Diretório no compartilhamento de rede"
          "demandaDiretorio"
          "Ex: 2026/Caso_042/Evidencia_01"
          True
        div_ [class_ "grid"] $ do
          campoTexto
            "Nome do produto final"
            "nomeProdutoFinal"
            "Ex: Relatorio_Caso_042"
            True
          label_ [for_ "perfilIPED"] $ do
            "Perfil IPED"
            select_ [id_ "perfilIPED", name_ "perfilIPED"] $ do
              option_ [value_ "default"] "Default"
              option_ [value_ "fastmode"] "Fastmode"
              option_ [value_ "forensic"] "Forensic"
          label_ $ do
            input_ [type_ "checkbox", name_ "urgente", value_ "true"]
            " Demanda Urgente (priorizar na fila de processamento)"

        campoCheckbox "Executar geração de relatório no PA" "gerarRelatorioPA" True
        campoCheckbox "Executar indexação no IPED" "executarIndexacaoIPED" True

      button_ [type_ "submit"] "Enviar para Fila"
