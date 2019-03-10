module MonadPixel
  ( MakePixel
  , MakeMonadPixel
  , runPixel
  ) where

--------------------------------------------------------------------------------

import Protolude
import Servant

--------------------------------------------------------------------------------

-- Pixel Monad Transformer
--
-- We're relying on Servant's ability to hoist our endpoints into our own
-- custom monad. Rather than provide a concrete monad, here we have type level
-- "functions" that will product a new concrete and MTL style monad class from
-- an error and configuration type.
--
-- Example:
--
--   ```
--   type Pixel = MakePixel ApplicationError Configuration
--   ```
--
type MakePixel err context =
  ExceptT err
    (ReaderT context IO)


type MakeMonadPixel err context m =
  ( MonadIO m
  , MonadReader context m
  , MonadError err m
  )


-- This is our Monad Transformer runner, note that we unwrap to Handler and not
-- IO, this means once fully unwrapped we still need to liftIO into Servant's
-- monad.
runPixel
  :: Show err
  => config
  -> MakePixel err config a
  -> Handler a

runPixel config m =
  unwrapPixel m >>= flip either pure (\err -> do
    putText $ "[PIXEL] Exception: " <> show err
    throwError err500 { errBody = "Oops!" }
  )

  where
    unwrapPixel = liftIO . flip runReaderT config . runExceptT
