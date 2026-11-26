module Main where

import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stderr, stdout, utf8)

import Ambiente (novoAmbiente)
import Domain.Types (WorkerId (..))
import Sessao (manterConexao)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- Logs imediatos mesmo quando a saída é redirecionada para arquivo ou pipe
  hSetBuffering stdout LineBuffering
  args <- getArgs
  (host, widTexto) <- case args of
    [h, w] -> pure (h, T.pack w)
    _ -> do
      putStrLn "Uso: worker.exe <IP_ORQUESTRADOR> <WORKER_ID>"
      exitFailure
  -- Resultados pendentes ficam ao lado do executável (o diretório de trabalho
  -- de uma tarefa agendada pode ser, por exemplo, C:\Windows\System32).
  dirPendentes <- (</> "resultados-pendentes") . takeDirectory <$> getExecutablePath
  createDirectoryIfMissing True dirPendentes
  amb <- novoAmbiente (WorkerId widTexto) dirPendentes
  manterConexao host amb
