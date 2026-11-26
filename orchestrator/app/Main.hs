{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import Web.Scotty (scottyApp)

import AppState (novoAppState)
import Configuracao (portaOrquestrador)
import Database (inicializarBanco)
import Despachante (loopDespachante)
import Log (registrar)
import Reconciliacao (loopVarreduraOrfas)
import Web.Rotas (routes)
import Ws (limiteSilencioSegundos, wsHandler)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- Logs imediatos mesmo quando a saída é redirecionada (arquivo, pipe, journald)
  hSetBuffering stdout LineBuffering

  pool  <- inicializarBanco
             ("dados" </> "orquestrador.db")
             ("orchestrator" </> "sql" </> "schema.sql")
  state <- novoAppState pool

  registrar "Orquestrador" $ "Escutando HTTP e WebSocket em 0.0.0.0:" <> T.pack (show portaOrquestrador)

  app <- scottyApp $ routes state

  -- Loop que verifica demandas e aloca para máquinas livres
  void $ forkIO (loopDespachante state)

  -- Loop que marca como erro as demandas de workers que sumiram
  void $ forkIO (loopVarreduraOrfas state)

  -- Warp escuta em todas as interfaces (0.0.0.0) na porta configurada. O timeout de
  -- inatividade do Warp (padrão 30 s) também derruba conexões WebSocket; ele fica
  -- 15 s acima de 'limiteSilencioSegundos' para que a detecção de worker morto
  -- seja a da aplicação, que registra o motivo no log.
  let configWarp = Warp.setPort portaOrquestrador
                 $ Warp.setTimeout (limiteSilencioSegundos + 15) Warp.defaultSettings
  Warp.runSettings configWarp $
    websocketsOr WS.defaultConnectionOptions (wsHandler state) app
