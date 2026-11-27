{-# LANGUAGE OverloadedStrings #-}

-- | Estado compartilhado entre as partes do worker.
module Ambiente
  ( Ambiente (..)
  , novoAmbiente
  , ambWorkerId
  , comTravaDeEstado
  , atualizarSituacaoSessao
  , estadoParaInformar
  , enviarSePossivel
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import qualified Data.Aeson as Aeson
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import ConfigWorker (ConfigWorker (..))
import Domain.Protocol (MsgDoWorker)
import Domain.Types (EstadoWorker (..), WorkerId)
import Log (registrar)
import SessaoGrafica
import Util (tentar)

data Ambiente = Ambiente
  { ambConfig :: ConfigWorker
  , ambDirPendentes :: FilePath
    -- ^ Pasta dos resultados ainda não confirmados pelo orquestrador.
  , ambDirLogs :: FilePath
    -- ^ Pasta dos logs de execução (saída do programa de cada demanda).
  , ambEstado :: TVar EstadoWorker
    -- ^ Livre ou Ocupada (o que o worker está fazendo). A disponibilidade da
    -- sessão gráfica é verificada à parte, ver 'estadoParaInformar'.
  , ambConexao :: TVar (Maybe WS.Connection)
    -- ^ Conexão atual com o orquestrador; Nothing enquanto desconectado.
  , ambTravaEstado :: MVar ()
    -- ^ Ver 'comTravaDeEstado'.
  , ambUltimaSessao :: IORef (Maybe SituacaoSessao)
    -- ^ Última situação da sessão gráfica, para registrar só as mudanças.
  }

-- | Ambiente inicial: worker Livre e sem conexão.
novoAmbiente :: ConfigWorker -> FilePath -> FilePath -> IO Ambiente
novoAmbiente cfg dirPendentes dirLogs =
  Ambiente cfg dirPendentes dirLogs <$> newTVarIO Livre <*> newTVarIO Nothing <*> newMVar () <*> newIORef Nothing

ambWorkerId :: Ambiente -> WorkerId
ambWorkerId = cfgWorkerId . ambConfig

-- | Executa uma ação com exclusão mútua sobre o estado informado ao orquestrador.
--
-- O heartbeat decide o estado e o envia dentro da trava; o recebimento de uma
-- demanda decide se a aceita e marca Ocupada dentro da trava. Assim um
-- heartbeat com um estado lido antes da demanda (Livre ou Indisponivel) nunca
-- é enviado depois da resposta a ela, o que faria o orquestrador liberar um
-- worker que está executando.
comTravaDeEstado :: Ambiente -> IO a -> IO a
comTravaDeEstado amb acao = withMVar (ambTravaEstado amb) (const acao)

-- | Verifica a sessão gráfica e registra no log quando a situação muda.
atualizarSituacaoSessao :: Ambiente -> IO SituacaoSessao
atualizarSituacaoSessao amb = do
  atual <- verificarSessaoGrafica
  anterior <- atomicModifyIORef' (ambUltimaSessao amb) (\a -> (Just atual, a))
  if anterior == Just atual
    then pure ()
    else registrar "Sessão gráfica" $ case atual of
      SessaoUtilizavel -> "Utilizável para automação."
      SessaoInutilizavel motivo -> "Inutilizável (o worker não aceitará demandas): " <> motivo <> "."
  pure atual

-- | Estado a informar ao orquestrador. Durante uma execução, é sempre Ocupada.
-- Fora dela, depende da sessão gráfica: Livre se ela permite automação,
-- Indisponivel com o motivo se não.
estadoParaInformar :: Ambiente -> IO EstadoWorker
estadoParaInformar amb = do
  estado <- readTVarIO (ambEstado amb)
  case estado of
    Ocupada _ -> pure estado
    _ -> do
      situacao <- atualizarSituacaoSessao amb
      pure $ case situacao of
        SessaoUtilizavel -> Livre
        SessaoInutilizavel motivo -> Indisponivel motivo

-- | Envia uma mensagem pela conexão atual, se houver. Como a conexão pode ser
-- trocada por uma reconexão durante a execução de uma demanda, quem executa
-- trabalho longo deve usar esta função em vez de guardar um 'WS.Connection'.
-- Falhas são registradas e não propagadas.
enviarSePossivel :: Ambiente -> MsgDoWorker -> IO ()
enviarSePossivel amb msg = do
  mConn <- readTVarIO (ambConexao amb)
  case mConn of
    Nothing -> registrar "Conexão" "Sem conexão com o orquestrador; mensagem não enviada."
    Just conn -> do
      r <- tentar (WS.sendTextData conn (Aeson.encode msg))
      case r of
        Left e -> registrar "Conexão" $ "Falha ao enviar mensagem: " <> T.pack (show e)
        Right () -> pure ()
