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
import qualified Configuration                 as C
import qualified Data.ByteString               as B
import qualified Data.UUID                     as U
import qualified Database.SQLite.Simple        as S
import qualified Eventless                     as E
import qualified Pixel                         as Pixel

--------------------------------------------------------------------------------

instance Pixel.MonadImage C.Pixel where
  saveImage  = pixelSaveImage
  loadImage  = pixelLoadImage
  loadImages = pixelLoadImages
  appendTags = pixelAppendTags
  removeTags = pixelRemoveTags

instance Pixel.MonadStatic C.Pixel where
  writeStaticImage = pixelWriteStaticImage

--------------------------------------------------------------------------------

-- Attempt to write image to Pixel monad's event sourced backend.
pixelSaveImage :: U.UUID -> Pixel.Image -> C.Pixel ()
pixelSaveImage uuid image = case image ^. Pixel.imageUploader of
  Nothing       -> pure ()
  Just userUUID ->
    view C.configConnection >>= \backend ->
      void $ E.runCommand backend uuid $ API.createImage userUUID image

-- Attempt to load image from Pixel monad's event sourced backend.
pixelLoadImage :: U.UUID -> C.Pixel (Maybe Pixel.Image)
pixelLoadImage uuid =
  map (^. E.value)
    <$> (view C.configConnection >>= flip E.loadLatest uuid)

-- Attempt to load all images from Pixel monad's event sourced backend.
pixelLoadImages :: Int -> C.Pixel [(U.UUID, Pixel.Image)]
pixelLoadImages _limit =
  view C.configReadSchema >>= \schema -> do
    images <- liftIO $ S.query_ schema
      "  SELECT i.uuid, i.hash, i.created \
       \ FROM images i                    \
       \ LIMIT 25"

    pure . catMaybes $ images <&> \(textUUID, imageHash, date) ->
      case U.fromText textUUID of
        Nothing   -> Nothing
        Just uuid -> Just
          ( uuid
          , Pixel.Image
            { Pixel._imageHash      = imageHash
            , Pixel._imageTags      = []
            , Pixel._imageUploader  = Nothing
            , Pixel._imageCreatedAt = Just date
            }
          )

-- Append tags
pixelAppendTags :: U.UUID -> API.TagList -> C.Pixel ()
pixelAppendTags uuid tags =
  view C.configConnection >>= \backend ->
    void . E.runCommand backend uuid $ API.addTags tags

-- Remove tags
pixelRemoveTags :: U.UUID -> API.TagList -> C.Pixel ()
pixelRemoveTags uuid tags =
  view C.configConnection >>= \backend ->
    void . E.runCommand backend uuid $ API.removeTags tags

-- Write static data to directory under the digest name.
pixelWriteStaticImage
  :: Text
  -> API.DigestText
  -> ByteString
  -> C.Pixel ()

pixelWriteStaticImage directory digest content =
  liftIO
    $ flip B.writeFile content
    $ fold [toS directory , "/" , toS digest]
