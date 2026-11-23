{-# LANGUAGE OverloadedStrings #-}

module Web.Views.Layout
  ( layoutBase
  , campoTexto
  , campoCheckbox
  ) where

import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttribute)

-- | Moldura HTML compartilhada por todas as páginas
layoutBase :: Text -> Html () -> Html ()
layoutBase titulo conteudo = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml titulo)
    link_
      [ rel_ "stylesheet"
      , href_ "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"
      ]
    script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ("" :: Text)
  body_ $
    main_ [class_ "container"] conteudo

-- | Helper para inputs de texto com rótulo
campoTexto :: Text -> Text -> Text -> Bool -> Html ()
campoTexto rotulo nomeCampo dica obrigatorio =
  label_ [for_ nomeCampo] $ do
    toHtml rotulo
    input_ $
      [ type_ "text"
      , id_ nomeCampo
      , name_ nomeCampo
      , placeholder_ dica
      ] <> [makeAttribute "required" "" | obrigatorio]

-- | Helper para checkboxes
campoCheckbox :: Text -> Text -> Bool -> Html ()
campoCheckbox rotulo nomeCampo marcadoPadrao =
  label_ [for_ nomeCampo] $ do
    input_ $
      [ type_ "checkbox"
      , id_ nomeCampo
      , name_ nomeCampo
      , value_ "true"
      ] <> [makeAttribute "checked" "" | marcadoPadrao]
    toHtml (" " <> rotulo)