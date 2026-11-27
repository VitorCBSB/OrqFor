{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stderr, stdout, utf8)

import Ambiente (novoAmbiente)
import ConfigWorker (carregarConfig)
import Log (registrar)
import Sessao (manterConexao)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- Logs imediatos mesmo quando a saída é redirecionada para arquivo ou pipe
  hSetBuffering stdout LineBuffering

  -- Arquivos do worker ficam ao lado do executável (o diretório de trabalho
  -- de uma tarefa agendada pode ser, por exemplo, C:\Windows\System32).
  dirExecutavel <- takeDirectory <$> getExecutablePath
  args <- getArgs
  caminhoConfig <- case args of
    []  -> pure (dirExecutavel </> "worker.json")
    [c] -> pure c
    _ -> do
      putStrLn "Uso: worker.exe [ARQUIVO_DE_CONFIGURACAO]"
      putStrLn "Sem argumento, lê worker.json na pasta do executável (ver worker.exemplo.json)."
      exitFailure

  resultado <- carregarConfig caminhoConfig
  cfg <- case resultado of
    Right cfg -> pure cfg
    Left erro -> do
      registrar "Configuração" erro
      exitFailure
  registrar "Configuração" $ "Lida de " <> T.pack caminhoConfig

  let dirPendentes = dirExecutavel </> "resultados-pendentes"
  createDirectoryIfMissing True dirPendentes
  amb <- novoAmbiente cfg dirPendentes
  manterConexao amb
