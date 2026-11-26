module Util
  ( tentar
  ) where

import qualified Control.Exception as EX

-- | Executa uma ação capturando exceções síncronas. Exceções assíncronas
-- (ex.: ThreadKilled no encerramento do programa) são propagadas.
tentar :: IO a -> IO (Either EX.SomeException a)
tentar acao = do
  r <- EX.try acao
  case r of
    Left e | Just (ae :: EX.SomeAsyncException) <- EX.fromException e -> EX.throwIO ae
    _ -> pure r
