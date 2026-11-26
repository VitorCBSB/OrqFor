{-# LANGUAGE OverloadedStrings #-}

-- | Registro de eventos (log) e formatação de valores do domínio para exibição.
module Log
  ( registrar
  , exibirDemanda
  , exibirEstado
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.IO (stdout)

import Domain.Types (DemandaId (..), EstadoWorker (..))

-- | Escreve uma linha de log no formato
-- @2026-11-26 16:05:12 [Componente] mensagem@, com o horário local.
--
-- A linha inteira é escrita com uma única operação na saída padrão, para que
-- linhas de threads diferentes não se misturem.
registrar :: Text -> Text -> IO ()
registrar componente mensagem = do
  agora <- getZonedTime
  let horario = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" agora)
      linha = horario <> " [" <> componente <> "] " <> mensagem <> "\n"
  BS.hPut stdout (TE.encodeUtf8 linha)

-- | Ex.: @#42@
exibirDemanda :: DemandaId -> Text
exibirDemanda (DemandaId n) = "#" <> T.pack (show n)

-- | Ex.: @Livre@, @Ocupada com a demanda #42@
exibirEstado :: EstadoWorker -> Text
exibirEstado Livre = "Livre"
exibirEstado (Ocupada did) = "Ocupada com a demanda " <> exibirDemanda did
