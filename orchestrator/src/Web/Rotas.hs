{-# LANGUAGE OverloadedStrings #-}

-- | Rotas HTTP da interface Web.
module Web.Rotas
  ( routes
  ) where

import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time (getCurrentTime)
import Lucid (renderText)
import Network.HTTP.Types.URI (renderSimpleQuery)
import Web.Scotty
import Web.Views.Demandas
import Web.Views.NovaDemanda (paginaNovaDemanda)

import AppState (AppState (..))
import Database (inserirDemandaDB, listarDemandasDB)
import Domain.Types
import Filtro

routes :: AppState -> ScottyM ()
routes state = do
  get "/" $ do
    filtro <- lerFiltro
    resultado <- liftIO $ listarDemandasDB (stDb state) filtro
    html $ renderText (paginaDemandas filtro resultado)

  -- Fragmento HTML da lista de demandas, consumido pelo htmx em "/" (polling e
  -- barra de filtros). O cabeçalho HX-Replace-Url atualiza o endereço da página
  -- com os filtros atuais, sem criar entradas no histórico do navegador.
  get "/fragmento-demandas" $ do
    filtro <- lerFiltro
    resultado <- liftIO $ listarDemandasDB (stDb state) filtro
    setHeader "HX-Replace-Url" (TL.fromStrict (urlDaPagina filtro))
    html $ renderText (fragmentoListaDemandas filtro resultado)

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

-- | Lê os filtros da URL. Valores ausentes ou desconhecidos viram o padrão.
lerFiltro :: ActionM FiltroDemandas
lerFiltro = do
  busca    <- fromMaybe "" <$> queryParamMaybe "busca"
  situacao <- maybe Todas situacaoDoCodigo <$> queryParamMaybe "situacao"
  -- O navegador só envia o checkbox quando marcado
  urgentes <- isJust <$> (queryParamMaybe "urgentes" :: ActionM (Maybe T.Text))
  pure (FiltroDemandas (T.strip busca) situacao urgentes)

-- | Endereço da página inicial com os filtros, ex.: @/?busca=3194&situacao=erro@
urlDaPagina :: FiltroDemandas -> T.Text
urlDaPagina filtro =
  "/" <> TE.decodeUtf8 (renderSimpleQuery True
    [ (TE.encodeUtf8 k, TE.encodeUtf8 v) | (k, v) <- parametrosDoFiltro filtro ])

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
