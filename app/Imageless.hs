module Imageless
  ( MakeImageless
  , MakeMonadImageless
  , runImageless
  ) where

import Control.Monad.Classes
import Servant
import Protolude hiding
  ( MonadReader
  )

-- Imageless Monad Transformer
--
-- We're relying on Servant's ability to hoist our endpoints into our own
-- custom monad. Rather than provide a concrete monad, here we have type level
-- "functions" that will product a new concrete and MTL style monad class from
-- an error and configuration type.
--
-- Example:
--
--   ```
--   type Imageless = MakeImageless ApplicationError Configuration
--   ```
--
type MakeImageless err context =
  ExceptT err
    (ReaderT context IO)


type MakeMonadImageless err context m =
  ( MonadIO m
  , MonadReader context m
  , MonadError err m
  )


-- This is our Monad Transformer runner, note that we unwrap to Handler and not
-- IO, this means once fully unwrapped we still need to liftIO into Servant's
-- monad.
runImageless
  :: Show err
  => config
  -> MakeImageless err config a
  -> Handler a

runImageless config m =
  unwrapImageless m >>= flip either pure (\err -> do
    putText $ "[Imageless] Exception: " <> show err
    throwError err500
      { errBody = "Oops!"
      })

  where
    unwrapImageless = liftIO . flip runReaderT config . runExceptT
