module Services.Image
  ( pixelSaveImage
  , pixelLoadImage
  , pixelLoadImages
  , pixelAppendTags
  , pixelRemoveTags
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens

import qualified API.Image.Commands            as API
import qualified Configuration                 as C
import qualified Data.UUID                     as U
import qualified Database.SQLite.Simple        as S
import qualified Eventless                     as E
import qualified Pixel                         as Pixel

--------------------------------------------------------------------------------

pixelSaveImage
  :: MonadIO m
  => MonadReader C.Config m
  => U.UUID
  -> Pixel.Image
  -> m ()

pixelSaveImage uuid image = case image ^. Pixel.imageUploader of
  Nothing       -> pure ()
  Just userUUID -> do
    backend <- view C.configConnection
    let create = API.createImage userUUID image
    void $ E.runCommand backend uuid create


pixelLoadImage
  :: MonadIO m
  => MonadReader C.Config m
  => U.UUID
  -> m (Maybe Pixel.Image)

pixelLoadImage uuid = do
  backend <- view C.configConnection
  image   <- E.loadLatest backend uuid
  pure $ (^. E.value) <$> image


pixelLoadImages
  :: MonadIO m
  => MonadReader C.Config m
  => Int
  -> m [(U.UUID, Pixel.Image)]

pixelLoadImages _limit = do
  schema <- view C.configReadSchema
  images <- liftIO $ S.query_ schema
    " SELECT i.uuid, i.hash, i.created \
    \ FROM images i                    \
    \ LIMIT 25                         "

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


pixelAppendTags
  :: MonadIO m
  => MonadReader C.Config m
  => U.UUID
  -> Pixel.TagList
  -> m ()

pixelAppendTags uuid tags = do
  backend <- view C.configConnection
  void . E.runCommand backend uuid $ API.addTags tags


-- Remove tags
pixelRemoveTags
  :: MonadIO m
  => MonadReader C.Config m
  => U.UUID
  -> Pixel.TagList
  -> m ()

pixelRemoveTags uuid tags = do
  backend <- view C.configConnection
  void . E.runCommand backend uuid $ API.removeTags tags
