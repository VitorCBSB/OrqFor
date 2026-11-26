{-# LANGUAGE OverloadedStrings #-}

-- | Rotas HTTP da interface Web.
module Web.Rotas
  ( routes
  ) where

import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Lucid (renderText)
import Web.Scotty
import Web.Views.Demandas
import Web.Views.NovaDemanda (paginaNovaDemanda)

import AppState (AppState (..))
import Database (inserirDemandaDB, listarDemandasDB)
import Domain.Types

routes :: AppState -> ScottyM ()
routes state = do
  get "/" $ do
    demandas <- liftIO $ listarDemandasDB (stDb state)
    html $ renderText (paginaDemandas demandas)

  -- Fragmento HTML da lista de demandas, consumido pelo polling do htmx em "/"
  get "/fragmento-demandas" $ do
    demandas <- liftIO $ listarDemandasDB (stDb state)
    html $ renderText (fragmentoListaDemandas demandas)

  get "/nova-demanda" $
    html $ renderText paginaNovaDemanda

  post "/submeter-demanda" $ do
    nome      <- formParam "usuarioNome"
    matricula <- formParam "usuarioMatricula"
    email     <- formParam "usuarioEmail"
    perfil    <- formParam "perfilIPED"
    spNum     <- formParam "spNum"
    spAno     <- formParam "spAno"
    urgente   <- lerCheckbox "urgente"
    exportar  <- lerCheckbox "exportarIPED"

    let solicitante = Usuario
          { usuarioNome      = T.strip nome
          , usuarioMatricula = Matricula (T.strip matricula)
          , usuarioEmail     = Email (T.strip email)
          }
        config  = ConfigProcessamento
          { exportarIPED = exportar
          , perfilIPED   = readPerfilIPED perfil
          }

    agora <- liftIO getCurrentTime
    _ <- liftIO $ inserirDemandaDB (stDb state) solicitante config urgente (SolicitacaoPericial spNum spAno) agora
    redirect "/"

-- | Lê um checkbox enviado via POST (navegadores omitem campos desmarcados)
lerCheckbox :: T.Text -> ActionM Bool
lerCheckbox nome = isJust . lookup nome <$> formParams

readPerfilIPED :: T.Text -> IPEDProfile
readPerfilIPED "default" = IPEDDefault
readPerfilIPED "blind" = Blind
readPerfilIPED "fastmode" = Fastmode
readPerfilIPED "forensic" = Forensic
readPerfilIPED "pedo" = Pedo
readPerfilIPED "triage" = Triage
readPerfilIPED str = error $ "Perfil IPED desconhecido: " <> (T.unpack str)
