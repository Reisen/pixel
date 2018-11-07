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


type MakeImageless err context =
  ExceptT err
    (ReaderT context IO)


type MakeMonadImageless err context m =
  ( MonadIO m
  , MonadReader context m
  , MonadError err m
  )


runImageless
  :: Show err
  => config
  -> MakeImageless err config a
  -> Handler a

runImageless config m =
  unwrapImageless m >>= \case
    Right res -> pure res
    Left  err -> do
      liftIO $ putText $ "[Imageless] Unhandled Exception: " <> (show err)
      throwError $ err500
        { errBody = "Oops!"
        }

  where
    unwrapImageless endpoint =
        liftIO
      . flip runReaderT config
      . runExceptT
      $ endpoint
