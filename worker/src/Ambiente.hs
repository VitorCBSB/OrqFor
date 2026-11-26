{-# LANGUAGE OverloadedStrings #-}

-- | Estado compartilhado entre as partes do worker.
module Ambiente
  ( Ambiente (..)
  , novoAmbiente
  , enviarSePossivel
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS

import Domain.Protocol (MsgDoWorker)
import Domain.Types (EstadoWorker (..), WorkerId)
import Util (tentar)

data Ambiente = Ambiente
  { ambWorkerId :: WorkerId
  , ambDirPendentes :: FilePath
    -- ^ Pasta dos resultados ainda não confirmados pelo orquestrador.
  , ambEstado :: TVar EstadoWorker
  , ambConexao :: TVar (Maybe WS.Connection)
    -- ^ Conexão atual com o orquestrador; Nothing enquanto desconectado.
  }

-- | Ambiente inicial: worker Livre e sem conexão.
novoAmbiente :: WorkerId -> FilePath -> IO Ambiente
novoAmbiente wid dir = Ambiente wid dir <$> newTVarIO Livre <*> newTVarIO Nothing

-- | Envia uma mensagem pela conexão atual, se houver. Como a conexão pode ser
-- trocada por uma reconexão durante a execução de uma demanda, quem executa
-- trabalho longo deve usar esta função em vez de guardar um 'WS.Connection'.
-- Falhas são registradas e não propagadas.
enviarSePossivel :: Ambiente -> MsgDoWorker -> IO ()
enviarSePossivel amb msg = do
  mConn <- readTVarIO (ambConexao amb)
  case mConn of
    Nothing -> TIO.putStrLn "[Aviso] Sem conexão com o orquestrador; mensagem não enviada."
    Just conn -> do
      r <- tentar (WS.sendTextData conn (Aeson.encode msg))
      case r of
        Left e -> TIO.putStrLn $ "[Aviso] Falha ao enviar mensagem: " <> T.pack (show e)
        Right () -> pure ()
