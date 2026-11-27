{-# LANGUAGE OverloadedStrings #-}

-- | Execução de uma demanda: roda o programa configurado (ex.: o .bat que chama
-- o AutoHotkey) e reporta o resultado ao orquestrador.
module Execucao
  ( executarDemanda
  ) where

import Control.Concurrent.STM (atomically, writeTVar)
import qualified Control.Exception as EX
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (AppendMode), hClose, hFlush, hSetEncoding, openFile, utf8)
import System.Process

import Ambiente
import ConfigWorker (ConfigWorker (..))
import Domain.Protocol
import Domain.Types
import Log (exibirDemanda, registrar)
import Pendentes (gravarPendente)
import Util (tentar)

-- | Executa o programa configurado para a demanda e reporta o resultado.
--
-- O programa roda na pasta em que está (como num duplo clique em um .bat), e
-- tudo o que ele escreve na saída padrão e de erro vai para um log da demanda
-- em 'ambDirLogs'. O sucesso é decidido pelo código de saída: 0 é sucesso.
executarDemanda :: Ambiente -> Demanda -> IO ()
executarDemanda amb demanda = do
  let did = demandaId demanda
      cfg = ambConfig amb
      executavel = cfgExecutavel cfg
      argumentos = cfgArgumentos cfg
      arquivoLog = ambDirLogs amb </> ("demanda-" <> show (unDemandaId did) <> ".log")
      -- Grava em disco antes de enviar, para o resultado sobreviver a uma queda
      -- de conexão (ver módulo 'Pendentes').
      reportar msg = do
        gravarPendente (ambDirPendentes amb) did msg
        enviarSePossivel amb msg

  -- O 'finally' garante a volta a Livre em qualquer saída, inclusive exceções
  -- assíncronas (ex.: killThread), para o worker nunca ficar preso em Ocupada.
  (`EX.finally` atomically (writeTVar (ambEstado amb) Livre)) $ do
    registrar "Execução" $ "Demanda " <> exibirDemanda did <> ": executando "
      <> T.pack (unwords (executavel : argumentos)) <> " (saída em " <> T.pack arquivoLog <> ")"

    -- Exceções ao lançar o processo (ex.: executável removido depois da
    -- inicialização) são capturadas e viram falha.
    resultado <- tentar (rodarComLog executavel argumentos arquivoLog demanda)

    case resultado of
      Left e -> do
        let motivo = "Falha ao executar " <> T.pack executavel <> ": " <> T.pack (show e)
        registrar "Execução" motivo
        reportar (DemandaFalhou did motivo)

      Right ExitSuccess -> do
        registrar "Execução" $ "Demanda " <> exibirDemanda did <> ": programa terminou com sucesso."
        -- Provisório: o caminho real da saída virá do modelo de pastas compartilhadas.
        let saidaTeste = CaminhoCompartilhado "C:\\Users\\PA\\Desktop\\PA_Auto\\saida_teste"
        reportar (DemandaConcluida did saidaTeste)

      Right (ExitFailure codigo) -> do
        final <- finalDoLog arquivoLog
        let motivo = "O programa terminou com o código " <> T.pack (show codigo)
              <> " (log em " <> T.pack arquivoLog <> ")"
              <> (if T.null final then "" else ". Últimas linhas: " <> final)
        registrar "Execução" $ "Demanda " <> exibirDemanda did <> ": " <> motivo
        reportar (DemandaFalhou did motivo)

-- | Roda o programa com as saídas padrão e de erro gravadas no arquivo de log
-- (acrescentando, se a demanda já tiver sido executada antes nesta máquina).
-- A entrada padrão é fechada: um "pause" no .bat não trava a execução.
rodarComLog :: FilePath -> [String] -> FilePath -> Demanda -> IO ExitCode
rodarComLog executavel argumentos arquivoLog demanda = do
  agora <- getZonedTime
  h <- openFile arquivoLog AppendMode
  hSetEncoding h utf8
  TIO.hPutStrLn h $ "=== Demanda " <> exibirDemanda (demandaId demanda) <> " iniciada em "
    <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" agora)
    <> " | " <> T.pack (unwords (executavel : argumentos))
  hFlush h
  let processo = (proc executavel argumentos)
        { cwd = Just (takeDirectory executavel)
        , std_in = CreatePipe
        , std_out = UseHandle h
        , std_err = UseHandle h
        }
  -- 'withCreateProcess' fecha o handle do log e garante que o processo não
  -- fica órfão se esta thread for interrompida.
  withCreateProcess processo $ \mEntrada _ _ ph -> do
    mapM_ hClose mEntrada
    waitForProcess ph

-- | Últimas linhas do log, para incluir no motivo de uma falha. A saída de um
-- .bat costuma vir na página de código do console, não em UTF-8; bytes
-- inválidos são substituídos em vez de causar erro.
finalDoLog :: FilePath -> IO Text
finalDoLog arquivoLog = do
  r <- tentar (BS.readFile arquivoLog)
  pure $ case r of
    Left _ -> ""
    Right bytes ->
      let linhas = filter (not . T.null) . map T.strip . T.lines
                 $ TE.decodeUtf8With TEE.lenientDecode bytes
          ultimas = T.intercalate " | " (lastN 5 linhas)
      in if T.length ultimas > 600 then "..." <> T.takeEnd 600 ultimas else ultimas
  where
    lastN n xs = drop (length xs - n) xs
