{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- As instâncias ToField/FromField dos tipos do domínio ficam aqui, e não junto
-- dos tipos no projeto shared, para que o worker não dependa de sqlite-simple.
{-# OPTIONS_GHC -Wno-orphans #-}

module Database
  ( DBPool
  , DemandaResumo (..)
  , ProximaDemanda (..)
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
  , concluirDemandaDB
  , registrarFalhaDemandaDB
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (RowParser)
import Database.SQLite.Simple.ToField (ToField, toField)
import Domain.Types
import EstadoDemanda
import Filtro (FiltroDemandas (..), Situacao (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | Os newtypes do domínio são gravados e lidos como o valor que envolvem.
deriving newtype instance ToField DemandaId
deriving newtype instance FromField DemandaId
deriving newtype instance ToField WorkerId
deriving newtype instance FromField WorkerId
deriving newtype instance ToField Email
deriving newtype instance FromField Email
deriving newtype instance ToField Matricula
deriving newtype instance FromField Matricula
deriving newtype instance ToField CaminhoCompartilhado
deriving newtype instance FromField CaminhoCompartilhado

type DBPool = MVar Connection

-- | Tipo de leitura para exibir na tabela da interface Web (JOIN de demandas + usuarios + estados)
data DemandaResumo = DemandaResumo
  { drId            :: DemandaId
  , drUsuarioNome   :: Text
  , drMatricula     :: Matricula
  , drConfig        :: Either Text ConfigProcessamento
    -- ^ Left com o erro de decodificação, se o config_json estiver ilegível
  , drSPNum         :: Int
  , drSPAno         :: Int
  , drUrgente       :: Bool
  , drEstadoCodigo  :: Text
  , drEstadoDesc    :: Text
  , drWorkerId      :: Maybe WorkerId
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

    let drConfig = either (Left . T.pack) Right (decodificarConfig cfgTexto)

    pure DemandaResumo{..}

-- | Texto gravado na coluna @config_json@.
codificarConfig :: ConfigProcessamento -> Text
codificarConfig = decodeUtf8 . BL.toStrict . encode

-- | Lê a coluna @config_json@. Não há valor padrão: uma configuração ilegível é
-- um erro, porque processar a demanda com perfil ou opções diferentes dos
-- pedidos, sem ninguém perceber, é pior do que uma falha visível.
decodificarConfig :: Text -> Either String ConfigProcessamento
decodificarConfig = eitherDecodeStrict . encodeUtf8

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
  verificarEstados conn
  newMVar conn

-- | Confere se a tabela @estados_processamento@ corresponde ao mapeamento de
-- "EstadoDemanda". Se não corresponder (ex.: o schema.sql mudou e o código não),
-- a inicialização falha, em vez de o orquestrador gravar estados errados.
verificarEstados :: Connection -> IO ()
verificarEstados conn = do
  encontrados <- query_ conn "SELECT id, codigo FROM estados_processamento ORDER BY id;" :: IO [(Int, Text)]
  let esperados = sortOn fst [ (idEstado e, codigoEstado e) | e <- [minBound .. maxBound] ]
  when (encontrados /= esperados) $
    ioError $ userError $
      "A tabela estados_processamento não corresponde ao tipo EstadoDemanda. Esperado: "
        <> show esperados <> "; encontrado no banco: " <> show encontrados

-- | Condição @coluna IN (?, ?, ...)@ para uma lista de estados, com os parâmetros.
estadoEm :: Query -> [EstadoDemanda] -> (Query, [SQLData])
estadoEm coluna estados =
  ( coluna <> " IN (" <> Query (T.intercalate ", " (map (const "?") estados)) <> ")"
  , map toField estados )

-- | Lê um arquivo .sql e executa cada instrução separada por ';'
executarArquivoSQL :: Connection -> FilePath -> IO ()
executarArquivoSQL conn caminhoSQL = do
  bytes <- BS.readFile caminhoSQL
  let conteudo = decodeUtf8 bytes
      comandos = filter (not . T.null) $ map T.strip (T.splitOn ";" conteudo)
  withTransaction conn $
    mapM_ (\cmd -> execute_ conn (Query cmd)) comandos

-- | Insere/atualiza o solicitante e grava a nova demanda na fila
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
      (usuarioEmail, usuarioNome, usuarioMatricula, agora, agora)

    execute conn
      "INSERT INTO demandas \
      \  (usuario_email, estado_id, config_json, urgente, sp_numero, sp_ano, submetida_em) \
      \VALUES (?, ?, ?, ?, ?, ?, ?);"
      ( usuarioEmail
      , EstadoNaFila
      , codificarConfig cfg
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

-- | Lista as demandas que atendem ao filtro, das mais recentes para as mais
-- antigas, com os dados do usuário e a descrição do estado. Devolve o total de
-- demandas encontradas e no máximo 100 delas.
listarDemandasDB :: DBPool -> FiltroDemandas -> IO (Int, [DemandaResumo])
listarDemandasDB pool filtro = withDB pool $ \conn -> do
  let (clausulaWhere, parametros) = clausulaFiltro filtro
  contagem <- query conn
    ( "SELECT COUNT(*) \
      \FROM demandas d \
      \JOIN usuarios u ON u.email = d.usuario_email"
      <> clausulaWhere <> ";" )
    parametros
  linhas <- query conn
    ( "SELECT \
      \  d.id, u.nome, u.matricula, \
      \  d.config_json, d.urgente, d.sp_numero, d.sp_ano, e.codigo, e.descricao, d.worker_id, d.mensagem_erro, d.submetida_em \
      \FROM demandas d \
      \JOIN usuarios u ON u.email = d.usuario_email \
      \JOIN estados_processamento e ON e.id = d.estado_id"
      <> clausulaWhere <>
      " ORDER BY d.id DESC \
      \LIMIT 100;" )
    parametros
  let total = case contagem of
        [Only n] -> n
        _        -> length linhas
  pure (total, linhas)

-- | Monta o WHERE do filtro e os parâmetros correspondentes. Os valores digitados
-- pelo usuário entram sempre como parâmetros (?), nunca no texto do SQL.
clausulaFiltro :: FiltroDemandas -> (Query, [SQLData])
clausulaFiltro (FiltroDemandas busca situacao somenteUrgentes)
  | null condicoes = ("", [])
  | otherwise =
      ( Query (" WHERE " <> T.intercalate " AND " (map (fromQuery . fst) condicoes))
      , concatMap snd condicoes )
  where
    condicoes = catMaybes [condBusca, condSituacao, condUrgente]

    -- Nome, matrícula ou "numero/ano" da SP contendo o texto. '%' e '_' digitados
    -- são escapados para valerem como caracteres comuns no LIKE.
    condBusca
      | T.null busca = Nothing
      | otherwise =
          let padrao = toField ("%" <> escaparLike busca <> "%")
          in Just ( "(u.nome LIKE ? ESCAPE '\\' \
                    \OR u.matricula LIKE ? ESCAPE '\\' \
                    \OR (d.sp_numero || '/' || d.sp_ano) LIKE ? ESCAPE '\\')"
                  , [padrao, padrao, padrao] )

    condSituacao = estadoEm "d.estado_id" <$> case situacao of
      Todas        -> Nothing
      EmAndamento  -> Just estadosNaoConcluidos
      Enfileiradas -> Just [EstadoNaFila]
      Concluidas   -> Just [EstadoFinalizado]
      ComErro      -> Just [EstadoErro]

    condUrgente
      | somenteUrgentes = Just ("d.urgente = 1", [])
      | otherwise       = Nothing

    escaparLike = T.concatMap $ \c ->
      if c `elem` ("\\%_" :: String) then T.pack ['\\', c] else T.singleton c

-- | Verifica se o worker existe na tabela 'workers' e está com ativa = 1
validarWorkerDB :: DBPool -> WorkerId -> IO Bool
validarWorkerDB pool wid = withDB pool $ \conn -> do
  rows <- query conn
    "SELECT ativa FROM workers WHERE id = ?;"
    (Only wid) :: IO [Only Int]
  case rows of
    [Only 1] -> pure True
    _        -> pure False

-- | Atualiza o timestamp de último contato do worker
atualizarHeartbeatWorkerDB :: DBPool -> WorkerId -> UTCTime -> IO ()
atualizarHeartbeatWorkerDB pool wid agora = withDB pool $ \conn ->
  execute conn
    "UPDATE workers SET ultimo_heartbeat = ? WHERE id = ?;"
    (agora, wid)

-- | Resultado da busca pela próxima demanda da fila.
data ProximaDemanda
  = ProximaDemanda Demanda
  | ConfigIlegivel DemandaId Text
    -- ^ O config_json gravado não pôde ser lido (com o erro de decodificação).
    -- A demanda não pode ser processada sem saber o perfil e as opções pedidas.

-- | Priorize urgentes na fila (urgente = 1 primeiro, depois ordem de chegada por id)
buscarProximaDemandaFilaDB :: DBPool -> IO (Maybe ProximaDemanda)
buscarProximaDemandaFilaDB pool = withDB pool $ \conn -> do
  rows <- query conn
    "SELECT d.id, u.nome, u.matricula, u.email, d.config_json, d.urgente, d.sp_numero, d.sp_ano, d.submetida_em \
    \FROM demandas d \
    \JOIN usuarios u ON u.email = d.usuario_email \
    \WHERE d.estado_id = ? \
    \ORDER BY d.urgente DESC, d.id ASC \
    \LIMIT 1;"
    (Only EstadoNaFila)
  pure $ case rows of
    [(did, unome, umat, uemail, cfgTxt, urg, spNum, spAno, subEm)] -> Just $
      case decodificarConfig cfgTxt of
        Left erro -> ConfigIlegivel did (T.pack erro)
        Right cfg -> ProximaDemanda Demanda
          { demandaId          = did
          , demandaSolicitante = Usuario unome umat uemail
          , demandaConfig      = cfg
          , demandaSP          = SolicitacaoPericial spNum spAno
          , demandaUrgente     = urg
          , demandaCriadaEm    = subEm
          }
    _ -> Nothing

-- | Marca a demanda como alocada (INICIALIZANDO) e registra o worker
atribuirDemandaWorkerDB :: DBPool -> DemandaId -> WorkerId -> UTCTime -> IO ()
atribuirDemandaWorkerDB pool did wid agora = withDB pool $ \conn ->
  execute conn
    "UPDATE demandas SET worker_id = ?, estado_id = ?, iniciada_em = ? WHERE id = ?;"
    (wid, EstadoInicializando, agora, did)

-- | Desfaz a atribuição de uma demanda que não chegou ao worker, devolvendo-a
-- para NA_FILA. Só age se ela ainda estiver em INICIALIZANDO.
devolverDemandaParaFilaDB :: DBPool -> DemandaId -> IO ()
devolverDemandaParaFilaDB pool did = withDB pool $ \conn ->
  execute conn
    "UPDATE demandas SET worker_id = NULL, estado_id = ?, iniciada_em = NULL WHERE id = ? AND estado_id = ?;"
    (EstadoNaFila, did, EstadoInicializando)

-- | Demandas em execução ('estadosEmExecucao') com o worker atribuído e o último
-- heartbeat desse worker: (id da demanda, id do worker, último heartbeat).
listarDemandasEmAndamentoDB :: DBPool -> IO [(DemandaId, WorkerId, Maybe UTCTime)]
listarDemandasEmAndamentoDB pool = withDB pool $ \conn -> do
  let (condicao, parametros) = estadoEm "d.estado_id" estadosEmExecucao
  query conn
    ( "SELECT d.id, d.worker_id, w.ultimo_heartbeat \
      \FROM demandas d \
      \JOIN workers w ON w.id = d.worker_id \
      \WHERE " <> condicao <> ";" )
    parametros

-- | Marca como ERRO uma demanda órfã (o worker perdeu a demanda ou sumiu).
-- Só age se ela ainda estiver em execução, para não sobrescrever um resultado
-- que tenha chegado nesse meio-tempo.
marcarDemandaOrfaDB :: DBPool -> DemandaId -> Text -> UTCTime -> IO ()
marcarDemandaOrfaDB pool did motivo agora = withDB pool $ \conn -> do
  let (condicao, parametrosEstado) = estadoEm "estado_id" estadosEmExecucao
  execute conn
    ( "UPDATE demandas SET estado_id = ?, mensagem_erro = ?, concluida_em = ? \
      \WHERE id = ? AND " <> condicao <> ";" )
    ([toField EstadoErro, toField motivo, toField agora, toField did] <> parametrosEstado)

-- | Atualiza a etapa atual da demanda no banco. Só age se ela ainda estiver em
-- execução: uma atualização que chegue atrasada, depois de a demanda ter sido
-- finalizada ou marcada como órfã, não a faz voltar a "em execução".
-- Devolve False se a atualização foi ignorada.
atualizarEtapaDemandaDB :: DBPool -> DemandaId -> EtapaProcessamento -> IO Bool
atualizarEtapaDemandaDB pool did etapa = withDB pool $ \conn -> do
  let (condicao, parametrosEstado) = estadoEm "estado_id" estadosEmExecucao
  execute conn
    ( "UPDATE demandas SET estado_id = ? WHERE id = ? AND " <> condicao <> ";" )
    ([toField (estadoDaEtapa etapa), toField did] <> parametrosEstado)
  (> 0) <$> changes conn

-- | Marca a demanda como FINALIZADO, com o caminho da saída.
concluirDemandaDB :: DBPool -> DemandaId -> CaminhoCompartilhado -> UTCTime -> IO ()
concluirDemandaDB pool did saida agora =
  gravarFimDemanda pool did EstadoFinalizado (Just saida) Nothing agora

-- | Marca a demanda como ERRO, com o motivo informado pelo worker.
registrarFalhaDemandaDB :: DBPool -> DemandaId -> Text -> UTCTime -> IO ()
registrarFalhaDemandaDB pool did motivo agora =
  gravarFimDemanda pool did EstadoErro Nothing (Just motivo) agora

gravarFimDemanda
  :: DBPool
  -> DemandaId
  -> EstadoDemanda
  -> Maybe CaminhoCompartilhado
  -> Maybe Text
  -> UTCTime
  -> IO ()
gravarFimDemanda pool did estado saida erro agora = withDB pool $ \conn ->
  execute conn
    "UPDATE demandas SET estado_id = ?, caminho_saida = ?, mensagem_erro = ?, concluida_em = ? WHERE id = ?;"
    (estado, saida, erro, agora, did)
