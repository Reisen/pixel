module API.Image.Services
  ( MonadImage(..)
  , MonadStatic(..)
  , BaseDirectory
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens

import qualified API.Image.Commands            as API
import qualified API.Image.Types               as API
import qualified API.Token                     as API
import qualified Configuration                 as C
import qualified Crypto.Hash                   as H
import qualified Data.Aeson                    as A
import qualified Data.ByteString               as B
import qualified Data.UUID                     as U
import qualified Eventless                     as E

--------------------------------------------------------------------------------

-- Useful Tag Aliases for working with static data.
type BaseDirectory = Text

--------------------------------------------------------------------------------

-- Tagless Final provider for methods that deal with physical byte persistance
-- of images. The API uses hashes as target file names for easy deduplication.
class MonadStatic m where
  -- Given a static directory, a hash of the file, and the contents itself,
  -- persist the data to disk.
  writeStaticImage
    :: BaseDirectory
    -> API.DigestText
    -> ByteString
    -> m ()

  -- Given a static directory, and a file hash, read the bytes from disk into
  -- memory.
  readStaticImage
    :: BaseDirectory
    -> API.DigestText
    -> m ByteString

--------------------------------------------------------------------------------

-- Tagless Final provider for methods that deal with metadata persistance of
-- images. The API must be storage agnostic.
class MonadImage m where
  -- Persist an image in the database, this assumes a brand new image each time
  -- and so always produces a new UUID for the recorded image.
  saveImage :: U.UUID -> API.Image -> m ()

  -- Attempts to retrieve a saved image from somewhere.
  loadImage :: U.UUID -> m (Maybe API.Image)

  -- Retrieve all images (lmited to a max) from backend.
  loadImages :: Int -> m [(U.UUID, API.Image)]

--------------------------------------------------------------------------------

instance MonadImage C.Pixel where
  saveImage  = pixelSaveImage
  loadImage  = pixelLoadImage
  loadImages = pixelLoadImages

instance MonadStatic C.Pixel where
  writeStaticImage = pixelWriteStaticImage
  readStaticImage  = undefined

--------------------------------------------------------------------------------

-- Attempt to write image to Pixel monad's event sourced backend.
pixelSaveImage :: U.UUID -> API.Image -> C.Pixel ()
pixelSaveImage uuid image = case image ^. API.imageUploader of
  Nothing       -> undefined
  Just userUUID -> do
    view C.configConnection >>= \backend ->
      void $ E.runCommand backend uuid $ API.createImage userUUID image

-- Attempt to load image from Pixel monad's event sourced backend.
pixelLoadImage :: U.UUID -> C.Pixel (Maybe API.Image)
pixelLoadImage uuid =
  map (^. E.value)
    <$> (view C.configConnection >>= flip E.loadLatest uuid)

-- Attempt to load all images from Pixel monad's event sourced backend.
pixelLoadImages :: Int -> C.Pixel [(U.UUID, API.Image)]
pixelLoadImages limit = undefined

-- Write static data to directory under the digest name.
pixelWriteStaticImage
  :: BaseDirectory
  -> API.DigestText
  -> ByteString
  -> C.Pixel ()

pixelWriteStaticImage directory digest content =
  liftIO
    $ flip B.writeFile content
    $ fold [toS directory , "/" , toS digest]
