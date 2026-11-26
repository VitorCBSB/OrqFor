{-# LANGUAGE OverloadedStrings #-}

-- | Execução de uma demanda via AutoHotkey.
module Execucao
  ( executarScriptTeste
  ) where

import Control.Concurrent.STM (atomically, writeTVar)
import qualified Control.Exception as EX
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)

import Ambiente
import Domain.Protocol
import Domain.Types
import Log (registrar)
import Pendentes (gravarPendente)
import Util (tentar)

-- | Executa o script AutoHotkey de teste e reporta o resultado ao Orquestrador
executarScriptTeste :: Ambiente -> Demanda -> IO ()
executarScriptTeste amb demanda = do
  let did = demandaId demanda
      caminhoAHK = "C:\\Users\\PA\\Desktop\\PA_Auto\\test.ahk"
      -- Se o AutoHotkey.exe não estiver no PATH do Windows, use o caminho completo:
      -- ex: "C:\\Program Files\\AutoHotkey\\v2\\AutoHotkey64.exe"
      executavelAHK = "C:\\Users\\PA\\Desktop\\PA_Auto\\deps\\AutoHotKey\\AutoHotkey64.exe"
      -- Grava em disco antes de enviar, para o resultado sobreviver a uma queda
      -- de conexão (ver módulo 'Pendentes').
      reportar msg = do
        gravarPendente (ambDirPendentes amb) did msg
        enviarSePossivel amb msg

  -- O 'finally' garante a volta a Livre em qualquer saída, inclusive exceções
  -- assíncronas (ex.: killThread), para o worker nunca ficar preso em Ocupada.
  (`EX.finally` atomically (writeTVar (ambEstado amb) Livre)) $ do
    registrar "Execução" $ "Disparando AutoHotkey: " <> T.pack caminhoAHK

    -- Executa de forma síncrona (aguarda o script terminar). Exceções ao lançar
    -- o processo (ex.: executável inexistente) são capturadas e viram falha.
    resultado <- tentar $
      readCreateProcessWithExitCode (proc executavelAHK [caminhoAHK]) ""

    case resultado of
      Left e -> do
        let motivo = "Falha ao executar o AutoHotkey: " <> T.pack (show e)
        registrar "Execução" motivo
        reportar (DemandaFalhou did motivo)

      Right (ExitSuccess, _stdout, _stderr) -> do
        registrar "Execução" "Script AutoHotkey finalizado com sucesso."
        let saidaTeste = CaminhoCompartilhado "C:\\Users\\PA\\Desktop\\PA_Auto\\saida_teste"
        reportar (DemandaConcluida did saidaTeste)

      Right (ExitFailure codigo, _stdout, stderrStr) -> do
        let motivo = "AutoHotkey retornou erro (código " <> T.pack (show codigo) <> "): " <> T.pack stderrStr
        registrar "Execução" motivo
        reportar (DemandaFalhou did motivo)
