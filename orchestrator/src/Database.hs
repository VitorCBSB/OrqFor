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
  , atribuirDemandaWorkerDB
  , devolverDemandaParaFilaDB
  , listarDemandasEmAndamentoDB
  , marcarDemandaOrfaDB
  , atualizarEtapaDemandaDB
  , finalizarDemandaDB
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
  , drConfig        :: ConfigProcessamento
  , drSPNum         :: Int
  , drSPAno         :: Int
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
    cfgTexto       <- field :: RowParser Text
    drUrgente      <- field
    drSPNum        <- field
    drSPAno        <- field
    drEstadoCodigo <- field
    drEstadoDesc   <- field
    drWorkerId     <- field
    drMensagemErro <- field
    drSubmetidaEm  <- field

    let cfgPadrao = ConfigProcessamento False IPEDDefault
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
  -> ConfigProcessamento
  -> Bool
  -> SolicitacaoPericial
  -> UTCTime
  -> IO Demanda
inserirDemandaDB pool usr@Usuario{..} cfg urgente sp agora = withDB pool $ \conn ->
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
      \  (usuario_email, estado_id, config_json, urgente, sp_numero, sp_ano, submetida_em) \
      \VALUES (?, 1, ?, ?, ?, ?, ?);"
      ( unEmail usuarioEmail
      , configJsonTexto
      , urgente
      , numero sp
      , ano sp
      , agora
      )

    novoId <- lastInsertRowId conn
    pure Demanda
      { demandaId          = DemandaId novoId
      , demandaSolicitante = usr
      , demandaConfig      = cfg
      , demandaSP          = sp
      , demandaUrgente     = urgente
      , demandaCriadaEm    = agora
      }

-- | Lista as demandas mais recentes com os dados do usuário e a descrição do estado
listarDemandasDB :: DBPool -> IO [DemandaResumo]
listarDemandasDB pool = withDB pool $ \conn ->
  query_ conn
    "SELECT \
    \  d.id, u.nome, u.matricula, \
    \  d.config_json, d.urgente, d.sp_numero, d.sp_ano, e.codigo, e.descricao, d.worker_id, d.mensagem_erro, d.submetida_em \
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
    "SELECT d.id, u.nome, u.matricula, u.email, d.config_json, d.urgente, d.sp_numero, d.sp_ano, d.submetida_em \
    \FROM demandas d \
    \JOIN usuarios u ON u.email = d.usuario_email \
    \WHERE d.estado_id = 1 \
    \ORDER BY d.urgente DESC, d.id ASC \
    \LIMIT 1;"
  case rows of
    [(did, unome, umat, uemail, cfgTxt, urg, spNum, spAno, subEm)] -> do
      let cfgPadrao = ConfigProcessamento False IPEDDefault
          cfg = fromMaybe cfgPadrao (decodeStrict (encodeUtf8 cfgTxt))
      pure $ Just Demanda
        { demandaId          = DemandaId did
        , demandaSolicitante = Usuario unome (Matricula umat) (Email uemail)
        , demandaConfig      = cfg
        , demandaSP          = SolicitacaoPericial spNum spAno
        , demandaUrgente     = urg
        , demandaCriadaEm    = subEm
        }
    _ -> pure Nothing

-- | Marca a demanda como alocada (estado 2 = INICIALIZANDO) e registra o worker
atribuirDemandaWorkerDB :: DBPool -> Int64 -> Text -> UTCTime -> IO ()
atribuirDemandaWorkerDB pool did wid agora = withDB pool $ \conn ->
  execute conn
    "UPDATE demandas SET worker_id = ?, estado_id = 2, iniciada_em = ? WHERE id = ?;"
    (wid, agora, did)

-- | Desfaz a atribuição de uma demanda que não chegou ao worker, devolvendo-a
-- para NA_FILA. Só age se ela ainda estiver em INICIALIZANDO (estado 2).
devolverDemandaParaFilaDB :: DBPool -> Int64 -> IO ()
devolverDemandaParaFilaDB pool did = withDB pool $ \conn ->
  execute conn
    "UPDATE demandas SET worker_id = NULL, estado_id = 1, iniciada_em = NULL WHERE id = ? AND estado_id = 2;"
    (Only did)
-- | Demandas em andamento (estados 2, 3 e 4) com o worker atribuído e o último
-- heartbeat desse worker: (id da demanda, id do worker, último heartbeat).
listarDemandasEmAndamentoDB :: DBPool -> IO [(Int64, Text, Maybe UTCTime)]
listarDemandasEmAndamentoDB pool = withDB pool $ \conn ->
  query_ conn
    "SELECT d.id, d.worker_id, w.ultimo_heartbeat \
    \FROM demandas d \
    \JOIN workers w ON w.id = d.worker_id \
    \WHERE d.estado_id IN (2, 3, 4);"

-- | Marca como ERRO uma demanda órfã (o worker perdeu a demanda ou sumiu).
-- Só age se ela ainda estiver em andamento, para não sobrescrever um resultado
-- que tenha chegado nesse meio-tempo.
marcarDemandaOrfaDB :: DBPool -> Int64 -> Text -> UTCTime -> IO ()
marcarDemandaOrfaDB pool did motivo agora = withDB pool $ \conn ->
  execute conn
    "UPDATE demandas SET estado_id = 6, mensagem_erro = ?, concluida_em = ? \
    \WHERE id = ? AND estado_id IN (2, 3, 4);"
    (motivo, agora, did)

-- | Atualiza apenas a etapa atual da demanda no banco
atualizarEtapaDemandaDB :: DBPool -> Int64 -> EtapaProcessamento -> IO ()
atualizarEtapaDemandaDB pool did etapa = withDB pool $ \conn ->
  execute conn
    "UPDATE demandas SET estado_id = ? WHERE id = ?;"
    (etapaParaId etapa, did)

-- | Atualiza o estado final (5 = FINALIZADO, 6 = ERRO)
finalizarDemandaDB 
  :: DBPool 
  -> Int64 
  -> EtapaProcessamento
  -> Maybe Text 
  -> Maybe Text 
  -> UTCTime 
  -> IO ()
finalizarDemandaDB pool did etapaFinal saida erro agora = withDB pool $ \conn ->
  execute conn
    "UPDATE demandas SET estado_id = ?, caminho_saida = ?, mensagem_erro = ?, concluida_em = ? WHERE id = ?;"
    (etapaParaId etapaFinal, saida, erro, agora, did)

etapaParaId :: EtapaProcessamento -> Int
etapaParaId NaFila = 1
etapaParaId Inicializando = 2
etapaParaId ExecutandoPA = 3
etapaParaId ExecutandoIPED = 4
etapaParaId Finalizado = 5
etapaParaId (Erro _) = 6
