{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Verifica se a sessão gráfica em que o worker roda permite automação de
-- interface (AutoHotkey). Ela não permite quando:
--
-- * o worker roda na sessão 0 (sessão de serviços, sem área de trabalho);
-- * a sessão não está ativa (ex.: desconectada porque a conexão de área de
--   trabalho remota foi fechada);
-- * a área de trabalho do usuário não está em primeiro plano (tela bloqueada,
--   proteção de tela com senha, tela do Ctrl+Alt+Del ou do UAC).
module SessaoGrafica
  ( SituacaoSessao (..)
  , verificarSessaoGrafica
  ) where

import Data.Text (Text)
#if defined(mingw32_HOST_OS)
import qualified Data.Text as T
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
#endif

data SituacaoSessao
  = SessaoUtilizavel
  | SessaoInutilizavel Text  -- ^ com o motivo, para log e para o orquestrador
  deriving (Show, Eq)

verificarSessaoGrafica :: IO SituacaoSessao

#if defined(mingw32_HOST_OS)

foreign import ccall unsafe "GetCurrentProcessId"
  c_GetCurrentProcessId :: IO Word32
foreign import ccall unsafe "ProcessIdToSessionId"
  c_ProcessIdToSessionId :: Word32 -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "WTSQuerySessionInformationW"
  c_WTSQuerySessionInformationW :: Ptr () -> Word32 -> CInt -> Ptr (Ptr ()) -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "WTSFreeMemory"
  c_WTSFreeMemory :: Ptr () -> IO ()
foreign import ccall unsafe "OpenInputDesktop"
  c_OpenInputDesktop :: Word32 -> CInt -> Word32 -> IO (Ptr ())
foreign import ccall unsafe "SwitchDesktop"
  c_SwitchDesktop :: Ptr () -> IO CInt
foreign import ccall unsafe "CloseDesktop"
  c_CloseDesktop :: Ptr () -> IO CInt
foreign import ccall unsafe "GetLastError"
  c_GetLastError :: IO Word32

verificarSessaoGrafica = do
  mSessao <- sessaoDoProcesso
  case mSessao of
    Left erro -> pure (falhaNaVerificacao "ProcessIdToSessionId" erro)
    Right 0 -> pure $ SessaoInutilizavel
      "o worker está na sessão 0 (sessão de serviços, sem área de trabalho); ele precisa rodar \
      \na sessão do usuário, por exemplo por uma tarefa agendada \"ao fazer logon\""
    Right sessao -> do
      mEstado <- estadoDaConexao sessao
      case mEstado of
        Left erro -> pure (falhaNaVerificacao "WTSQuerySessionInformation" erro)
        Right 0 -> verificarAreaDeTrabalho  -- 0 = WTSActive
        Right estado -> pure $ SessaoInutilizavel $
          "a sessão " <> T.pack (show sessao) <> " está " <> nomeDoEstado estado
            <> "; a automação exige uma sessão ativa (ao encerrar um acesso remoto, devolva \
               \a sessão ao console com tscon em vez de apenas fechar a janela)"

-- | Id da sessão do processo atual.
sessaoDoProcesso :: IO (Either Word32 Word32)
sessaoDoProcesso = do
  pid <- c_GetCurrentProcessId
  alloca $ \pSessao -> do
    ok <- c_ProcessIdToSessionId pid pSessao
    if ok /= 0 then Right <$> peek pSessao else Left <$> c_GetLastError

-- | Estado de conexão da sessão (WTS_CONNECTSTATE_CLASS).
estadoDaConexao :: Word32 -> IO (Either Word32 CInt)
estadoDaConexao sessao =
  alloca $ \pBuffer -> alloca $ \pTamanho -> do
    -- servidor local (NULL) e classe WTSConnectState (8)
    ok <- c_WTSQuerySessionInformationW nullPtr sessao 8 pBuffer pTamanho
    if ok == 0
      then Left <$> c_GetLastError
      else do
        buffer <- peek pBuffer
        estado <- peek (castPtr buffer :: Ptr CInt)
        c_WTSFreeMemory buffer
        pure (Right estado)

-- | Com a sessão ativa, confere se a área de trabalho do usuário é a que recebe
-- a entrada. Com a tela bloqueada (ou o Ctrl+Alt+Del/UAC aberto), a área de
-- trabalho de entrada é a segura, e abri-la ou ativá-la falha.
verificarAreaDeTrabalho :: IO SituacaoSessao
verificarAreaDeTrabalho = do
  desktop <- c_OpenInputDesktop 0 0 desktopSwitchDesktop
  if desktop == nullPtr
    then pure bloqueada
    else do
      ativou <- c_SwitchDesktop desktop
      _ <- c_CloseDesktop desktop
      pure (if ativou /= 0 then SessaoUtilizavel else bloqueada)
  where
    desktopSwitchDesktop = 0x0100
    bloqueada = SessaoInutilizavel
      "a área de trabalho do usuário não está em primeiro plano (tela bloqueada, proteção de \
      \tela com senha ou tela do Ctrl+Alt+Del/UAC aberta)"

nomeDoEstado :: CInt -> Text
nomeDoEstado estado = case estado of
  1 -> "conectada, mas sem usuário com logon"
  2 -> "em processo de conexão"
  3 -> "sendo espelhada (shadow)"
  4 -> "desconectada"
  5 -> "ociosa"
  6 -> "aguardando conexões"
  7 -> "sendo reiniciada"
  8 -> "inativa por erro"
  9 -> "inicializando"
  _ -> "em um estado desconhecido (" <> T.pack (show estado) <> ")"

-- | Se a própria verificação falhar, o worker se declara indisponível: é
-- preferível não receber demandas a falhar todas elas.
falhaNaVerificacao :: Text -> Word32 -> SituacaoSessao
falhaNaVerificacao funcao erro = SessaoInutilizavel $
  "não foi possível verificar a sessão gráfica (" <> funcao <> " falhou com o erro "
    <> T.pack (show erro) <> ")"

#else

-- | Fora do Windows não há o que verificar; existe só para o projeto compilar
-- em qualquer sistema (ex.: cabal build all na máquina Linux do orquestrador).
verificarSessaoGrafica = pure SessaoUtilizavel

#endif
