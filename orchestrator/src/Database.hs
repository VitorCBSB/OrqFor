{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database
  ( DBPool
  , DemandaResumo (..)
  , inicializarBanco
  , inserirDemandaDB
  , listarDemandasDB
  , validarWorkerDB
  , atualizarHeartbeatWorkerDB
  , buscarProximaDemandaFilaDB
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (RowParser)
import Domain.Types
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

type DBPool = MVar Connection

-- | Tipo de leitura para exibir na tabela da interface Web (JOIN de demandas + usuarios + estados)
data DemandaResumo = DemandaResumo
  { drId            :: Int64
  , drUsuarioNome   :: Text
  , drMatricula     :: Text
  , drDiretorio     :: Text
  , drProdutoFinal  :: Text
  , drConfig        :: ConfigProcessamento
  , drUrgente       :: Bool
  , drEstadoCodigo  :: Text
  , drEstadoDesc    :: Text
  , drWorkerId      :: Maybe Text
  , drMensagemErro  :: Maybe Text
  , drSubmetidaEm   :: UTCTime
  } deriving (Show)

instance FromRow DemandaResumo where
  fromRow = do
    drId           <- field
    drUsuarioNome  <- field
    drMatricula    <- field
    drDiretorio    <- field
    drProdutoFinal <- field
    cfgTexto       <- field :: RowParser Text
    drUrgente      <- field
    drEstadoCodigo <- field
    drEstadoDesc   <- field
    drWorkerId     <- field
    drMensagemErro <- field
    drSubmetidaEm  <- field

    let cfgPadrao = ConfigProcessamento drProdutoFinal False False "default" []
        drConfig  = fromMaybe cfgPadrao (decodeStrict (encodeUtf8 cfgTexto))

    pure DemandaResumo{..}

-- | Helper para executar operações no SQLite com exclusão mútua segura entre threads
withDB :: DBPool -> (Connection -> IO a) -> IO a
withDB = withMVar

-- | Abre o arquivo SQLite, configura PRAGMAs, cria tabelas e popula estados iniciais
inicializarBanco :: FilePath -> FilePath -> IO DBPool
inicializarBanco caminhoDB caminhoSchema = do
  createDirectoryIfMissing True (takeDirectory caminhoDB)
  conn <- open caminhoDB

  execute_ conn "PRAGMA journal_mode = WAL;"
  execute_ conn "PRAGMA synchronous = NORMAL;"
  execute_ conn "PRAGMA foreign_keys = ON;"
  execute_ conn "PRAGMA busy_timeout = 5000;"

  executarArquivoSQL conn caminhoSchema
  newMVar conn

-- | Lê um arquivo .sql e executa cada instrução separada por ';'
executarArquivoSQL :: Connection -> FilePath -> IO ()
executarArquivoSQL conn caminhoSQL = do
  bytes <- BS.readFile caminhoSQL
  let conteudo = decodeUtf8 bytes
      comandos = filter (not . T.null) $ map T.strip (T.splitOn ";" conteudo)
  withTransaction conn $
    mapM_ (\cmd -> execute_ conn (Query cmd)) comandos

-- | Insere/atualiza o solicitante e grava a nova demanda com estado inicial 1 (NA_FILA)
inserirDemandaDB
  :: DBPool
  -> Usuario
  -> CaminhoCompartilhado
  -> ConfigProcessamento
  -> Bool
  -> UTCTime
  -> IO Demanda
inserirDemandaDB pool usr@Usuario{..} caminho cfg urgente agora = withDB pool $ \conn ->
  withTransaction conn $ do
    -- Upsert do usuário solicitante
    execute conn
      "INSERT INTO usuarios (email, nome, matricula, criado_em, ultimo_acesso) \
      \VALUES (?, ?, ?, ?, ?) \
      \ON CONFLICT(email) DO UPDATE SET \
      \  nome = excluded.nome, \
      \  matricula = excluded.matricula, \
      \  ultimo_acesso = excluded.ultimo_acesso;"
      (unEmail usuarioEmail, usuarioNome, unMatricula usuarioMatricula, agora, agora)

    let configJsonTexto = decodeUtf8 (BL.toStrict (encode cfg))

    execute conn
      "INSERT INTO demandas \
      \  (usuario_email, estado_id, diretorio_entrada, nome_produto_final, config_json, urgente, submetida_em) \
      \VALUES (?, 1, ?, ?, ?, ?, ?);"
      ( unEmail usuarioEmail
      , unCaminho caminho
      , nomeProdutoFinal cfg
      , configJsonTexto
      , urgente
      , agora
      )

    novoId <- lastInsertRowId conn
    pure Demanda
      { demandaId          = DemandaId novoId
      , demandaSolicitante = usr
      , demandaDiretorio   = caminho
      , demandaConfig      = cfg
      , demandaUrgente     = urgente
      , demandaCriadaEm    = agora
      }

-- | Lista as demandas mais recentes com os dados do usuário e a descrição do estado
listarDemandasDB :: DBPool -> IO [DemandaResumo]
listarDemandasDB pool = withDB pool $ \conn ->
  query_ conn
    "SELECT \
    \  d.id, u.nome, u.matricula, d.diretorio_entrada, d.nome_produto_final, \
    \  d.config_json, e.codigo, e.descricao, d.worker_id, d.mensagem_erro, d.submetida_em \
    \FROM demandas d \
    \JOIN usuarios u ON u.email = d.usuario_email \
    \JOIN estados_processamento e ON e.id = d.estado_id \
    \ORDER BY d.id DESC \
    \LIMIT 100;"

-- | Verifica se o worker existe na tabela 'workers' e está com ativa = 1
validarWorkerDB :: DBPool -> Text -> IO Bool
validarWorkerDB pool workerId = withDB pool $ \conn -> do
  rows <- query conn
    "SELECT ativa FROM workers WHERE id = ?;"
    (Only workerId) :: IO [Only Int]
  case rows of
    [Only 1] -> pure True
    _        -> pure False

-- | Atualiza o timestamp de último contato do worker
atualizarHeartbeatWorkerDB :: DBPool -> Text -> UTCTime -> IO ()
atualizarHeartbeatWorkerDB pool workerId agora = withDB pool $ \conn ->
  execute conn
    "UPDATE workers SET ultimo_heartbeat = ? WHERE id = ?;"
    (agora, workerId)

-- | Priorize urgentes na fila (urgente = 1 primeiro, depois ordem de chegada por id)
buscarProximaDemandaFilaDB :: DBPool -> IO (Maybe Demanda)
buscarProximaDemandaFilaDB pool = withDB pool $ \conn -> do
  rows <- query_ conn
    "SELECT d.id, u.nome, u.matricula, u.email, d.diretorio_entrada, d.config_json, d.urgente, d.submetida_em \
    \FROM demandas d \
    \JOIN usuarios u ON u.email = d.usuario_email \
    \WHERE d.estado_id = 1 \
    \ORDER BY d.urgente DESC, d.id ASC \
    \LIMIT 1;"
  case rows of
    [(did, unome, umat, uemail, dir, cfgTxt, urg, subEm)] -> do
      let cfgPadrao = ConfigProcessamento "Teste" False False "default" []
          cfg = fromMaybe cfgPadrao (decodeStrict (encodeUtf8 cfgTxt))
      pure $ Just Demanda
        { demandaId          = DemandaId did
        , demandaSolicitante = Usuario unome (Matricula umat) (Email uemail)
        , demandaDiretorio   = CaminhoCompartilhado dir
        , demandaConfig      = cfg
        , demandaUrgente     = urg
        , demandaCriadaEm    = subEm
        }
    _ -> pure Nothing