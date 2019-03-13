module MonadPixel
  ( MakePixel
  , MakeMonadPixel
  , Pixel (..)
  , runPixel
  ) where

--------------------------------------------------------------------------------

import           Protolude
import           Servant

import qualified Configuration                 as C
import qualified Error                         as E
import qualified Pixel                         as Pixel
import qualified Services.Image                as Services
import qualified Services.Static               as Services

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
  { unwrapPixel :: MakePixel E.PixelError C.Config a
  } deriving newtype (MonadReader C.Config)
    deriving newtype (MonadError E.PixelError)
    deriving newtype MonadIO
    deriving newtype Functor
    deriving newtype Applicative
    deriving newtype Monad

-- Configure Pixel Instances
instance Pixel.MonadImage Pixel where
  saveImage  = Services.pixelSaveImage
  loadImage  = Services.pixelLoadImage
  loadImages = Services.pixelLoadImages
  appendTags = Services.pixelAppendTags
  removeTags = Services.pixelRemoveTags

instance Pixel.MonadStatic Pixel where
  writeStaticImage = Services.pixelWriteStaticImage

--------------------------------------------------------------------------------

-- This is our Monad Transformer runner, note that we unwrap to Handler and not
-- IO, this means once fully unwrapped we still need to liftIO into Servant's
-- monad.
runPixel
  :: C.Config
  -> Pixel a
  -> Handler a

runPixel config m =
  runner m >>= flip either pure (\err -> do
    putText $ "[PIXEL] Exception: " <> show err
    throwError err500 { errBody = "Oops!" }
  )

  where
    runner = liftIO . flip runReaderT config . runExceptT . unwrapPixel
