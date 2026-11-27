{-# LANGUAGE OverloadedStrings #-}

-- | Configuração de cada máquina worker, lida de um arquivo JSON na inicialização.
--
-- Fica aqui o que varia de máquina para máquina. O que precisa ser igual no
-- worker e no orquestrador (porta, caminho do WebSocket, heartbeat) fica no
-- módulo "Configuracao" do projeto shared.
module ConfigWorker
  ( ConfigWorker (..)
  , carregarConfig
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

import Domain.Types (WorkerId (..))
import Util (tentar)

-- | Exemplo de arquivo (worker.json):
--
-- > {
-- >   "orquestrador": "10.0.0.5",
-- >   "workerId": "WIN-WORKER-01",
-- >   "executavel": "C:\\Users\\PA\\Desktop\\PA_Auto\\run_ahk.bat",
-- >   "argumentos": []
-- > }
data ConfigWorker = ConfigWorker
  { cfgOrquestrador :: String
    -- ^ Endereço (IP ou nome) da máquina do orquestrador
  , cfgWorkerId     :: WorkerId
    -- ^ Precisa estar cadastrado e ativo na tabela workers do orquestrador
  , cfgExecutavel   :: FilePath
    -- ^ Programa executado para cada demanda (ex.: um .bat que chama o
    -- AutoHotkey, ou o próprio AutoHotkey64.exe). Roda na pasta em que está.
  , cfgArgumentos   :: [String]
    -- ^ Argumentos passados ao programa (ex.: o caminho de um script .ahk)
  } deriving (Show, Generic)

-- | Os campos do JSON são os nomes do record sem o prefixo "cfg"
-- (ex.: cfgWorkerId -> workerId). Campos desconhecidos são recusados, para que
-- um erro de digitação não passe despercebido.
instance Aeson.FromJSON ConfigWorker where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = minusculaInicial . drop (length ("cfg" :: String))
    , Aeson.rejectUnknownFields = True
    }
    where
      minusculaInicial (c : cs) = toLower c : cs
      minusculaInicial [] = []

-- | Lê e valida o arquivo de configuração. Em caso de problema, devolve uma
-- mensagem que explica o que corrigir.
--
-- O executável precisa existir: sem ele o worker aceitaria cada demanda e
-- falharia na hora, esvaziando a fila para ERRO.
carregarConfig :: FilePath -> IO (Either Text ConfigWorker)
carregarConfig caminho = do
  conteudo <- tentar (BL.readFile caminho)
  case conteudo of
    Left e -> pure $ Left $
      "Não foi possível ler o arquivo de configuração " <> T.pack caminho <> ": " <> T.pack (show e)
    Right bytes -> case Aeson.eitherDecode (semBom bytes) of
      Left erro -> pure $ Left $
        "Arquivo de configuração " <> T.pack caminho <> " inválido: " <> T.pack erro
      Right cfg -> validar cfg
  where
    -- O Bloco de Notas e o PowerShell 5 gravam UTF-8 com BOM, que o aeson não aceita.
    semBom bytes
      | BL.take 3 bytes == BL.pack [0xEF, 0xBB, 0xBF] = BL.drop 3 bytes
      | otherwise = bytes

    validar cfg = do
      executavelExiste <- doesFileExist (cfgExecutavel cfg)
      let vazios = [ campo | (campo, valor) <- [ ("orquestrador", T.pack (cfgOrquestrador cfg))
                                                , ("workerId", unWorkerId (cfgWorkerId cfg)) ]
                           , T.null (T.strip valor) ]
          problemas = [ "o campo \"" <> campo <> "\" está vazio" | campo <- vazios ]
                   <> [ "o arquivo do campo \"executavel\" não existe: " <> T.pack (cfgExecutavel cfg)
                      | not executavelExiste ]
      pure $ if null problemas
        then Right cfg
        else Left $ "Configuração em " <> T.pack caminho <> ": " <> T.intercalate "; " problemas <> "."
