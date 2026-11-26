{-# LANGUAGE OverloadedStrings #-}

-- | Resultados pendentes.
--
-- O resultado final de uma demanda ('DemandaConcluida'/'DemandaFalhou') é gravado
-- em disco antes de ser enviado e só é apagado quando o orquestrador responde
-- 'ResultadoRegistrado'. Assim ele sobrevive a quedas de conexão e a reinícios
-- do worker, e é reenviado a cada nova conexão até ser confirmado.
module Pendentes
  ( gravarPendente
  , listarPendentes
  , removerPendente
  ) where

import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, removeFile, renameFile)
import System.FilePath ((</>))

import Domain.Protocol
import Domain.Types
import Util (tentar)

arquivoPendente :: FilePath -> DemandaId -> FilePath
arquivoPendente dir (DemandaId did) = dir </> (show did <> ".json")

demandaDoResultado :: MsgDoWorker -> Maybe DemandaId
demandaDoResultado (DemandaConcluida did _) = Just did
demandaDoResultado (DemandaFalhou did _) = Just did
demandaDoResultado _ = Nothing

-- | Grava o resultado em disco. A gravação usa arquivo temporário + renomeação
-- para que um arquivo pela metade nunca seja lido como resultado. Uma falha é
-- registrada e não propagada: o resultado ainda é enviado, só sem reenvio garantido.
gravarPendente :: FilePath -> DemandaId -> MsgDoWorker -> IO ()
gravarPendente dir did msg = do
  let destino = arquivoPendente dir did
      temporario = destino <> ".tmp"
  gravado <- tentar $ do
    BL.writeFile temporario (Aeson.encode msg)
    renameFile temporario destino
  case gravado of
    Left e -> TIO.putStrLn $ "[Pendentes] Falha ao gravar resultado da demanda "
                <> T.pack (show (unDemandaId did)) <> " em disco (sem reenvio garantido): " <> T.pack (show e)
    Right () -> pure ()

-- | Lê os resultados ainda não confirmados. Arquivos ilegíveis são registrados e ignorados.
listarPendentes :: FilePath -> IO [(DemandaId, MsgDoWorker)]
listarPendentes dir = do
  nomes <- filter (".json" `isSuffixOf`) <$> listDirectory dir
  fmap catMaybes $ forM nomes $ \nome -> do
    conteudo <- tentar (BL.readFile (dir </> nome))
    case fmap Aeson.eitherDecode conteudo of
      Right (Right msg) | Just did <- demandaDoResultado msg -> pure (Just (did, msg))
      outro -> do
        let motivo = case outro of
              Left e -> show e
              Right (Left erro) -> erro
              Right (Right _) -> "não é um resultado de demanda"
        TIO.putStrLn $ "[Pendentes] Ignorando " <> T.pack nome <> ": " <> T.pack motivo
        pure Nothing

removerPendente :: FilePath -> DemandaId -> IO ()
removerPendente dir did = do
  r <- tentar (removeFile (arquivoPendente dir did))
  case r of
    Left e -> TIO.putStrLn $ "[Pendentes] Falha ao remover resultado confirmado: " <> T.pack (show e)
    Right () -> TIO.putStrLn $ "[Pendentes] Resultado da demanda " <> T.pack (show (unDemandaId did)) <> " confirmado pelo orquestrador."
