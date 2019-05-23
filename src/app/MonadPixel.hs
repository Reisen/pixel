module MonadPixel
  ( MakePixel
  , MakeMonadPixel
  , Pixel (..)
  , runPixel
  ) where

import Protolude
import Servant

import Configuration   ( Config )
import Pixel           ( Error(..)
                       , MonadImage(..)
                       , MonadStatic(..)
                       , MonadUser(..)
                       )
import Services.Image  ( pixelSaveImage
                       , pixelLoadImage
                       , pixelLoadImages
                       , pixelAppendTags
                       , pixelRemoveTags
                       )
import Services.Static ( pixelWriteStaticImage )
import Services.User   ( pixelCreateUser
                       , pixelFindRoleByName
                       , pixelFindUserByUUID
                       , pixelFindUserByEmail
                       )

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

-- Concrete Wrapper
newtype Pixel a = Pixel
  { unwrapPixel :: MakePixel Error Config a
  } deriving newtype (MonadReader Config)
    deriving newtype (MonadError Error)
    deriving newtype MonadIO
    deriving newtype Functor
    deriving newtype Applicative
    deriving newtype Monad

--------------------------------------------------------------------------------

-- Configure Pixel Instances
instance MonadImage Pixel where
  saveImage  = pixelSaveImage
  loadImage  = pixelLoadImage
  loadImages = pixelLoadImages
  appendTags = pixelAppendTags
  removeTags = pixelRemoveTags

instance MonadStatic Pixel where
  writeStaticImage = pixelWriteStaticImage

instance MonadUser Pixel where
  createUser      = pixelCreateUser
  findRoleByName  = pixelFindRoleByName
  findUserByEmail = pixelFindUserByEmail
  findUserByUUID  = pixelFindUserByUUID

--------------------------------------------------------------------------------

-- This is our Monad Transformer runner, note that we unwrap to Handler and not
-- IO, this means once fully unwrapped we still need to liftIO into Servant's
-- monad.
runPixel
  :: Config
  -> Pixel a
  -> Handler a

runPixel config m =
  runner m >>= flip either pure (\err -> do
    putText $ "[PIXEL] Exception: " <> show err
    throwError err500 { errBody = "Oops!" }
  )

  where
    runner = liftIO
      . flip runReaderT config
      . runExceptT
      . unwrapPixel
