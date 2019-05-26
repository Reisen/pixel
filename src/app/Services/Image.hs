module Services.Image
  ( pixelSaveImage
  , pixelLoadImage
  , pixelLoadImages
  , pixelAppendTags
  , pixelRemoveTags
  ) where

--------------------------------------------------------------------------------

import Protolude
import Control.Lens
import Text.InterpolatedString.QM
import API.Image.Commands            ( createImage, addTags, removeTags )
import Configuration                 ( Config, configConnection, configReadSchema )
import Data.UUID                     ( UUID, fromText )
import Data.Text                     ( splitOn )
import Database.SQLite.Simple        ( query_ )
import Eventless                     ( runCommand, loadLatest, value )
import Pixel.API.Images              ( Image(..), TagList, imageUploader )

--------------------------------------------------------------------------------

pixelSaveImage
  :: MonadIO m
  => MonadReader Config m
  => UUID
  -> Image
  -> m ()

pixelSaveImage uuid image = case image ^. imageUploader of
  Nothing       -> pure ()
  Just userUUID -> do
    backend <- view configConnection
    let create = createImage userUUID image
    void $ runCommand backend uuid create


pixelLoadImage
  :: MonadIO m
  => MonadReader Config m
  => UUID
  -> m (Maybe Image)

pixelLoadImage uuid = do
  backend <- view configConnection
  image   <- loadLatest backend uuid
  pure $ (^. value) <$> image


pixelLoadImages
  :: MonadIO m
  => MonadReader Config m
  => Int
  -> m [(UUID, Image)]

pixelLoadImages _limit = do
  schema <- view configReadSchema
  images <- liftIO $ query_ schema [qns|
      SELECT    i.uuid
              , i.hash
              , i.created
              , group_concat(ta.name) tags
      FROM      images i
      LEFT JOIN tag_associations ta ON i.uuid = ta.image
      GROUP BY  i.uuid
      ORDER BY  i.created DESC
  |]

  pure . catMaybes $ images <&> \(textUUID, imageHash, date, tags) ->
    case fromText textUUID of
      Nothing   -> Nothing
      Just uuid -> Just
        ( uuid
        , Image
          { _imageHash      = imageHash
          , _imageTags      = fromMaybe [] $ splitOn "," <$> tags
          , _imageUploader  = Nothing
          , _imageCreatedAt = Just date
          , _imageDeletedAt = Nothing
          }
        )


pixelAppendTags
  :: MonadIO m
  => MonadReader Config m
  => UUID
  -> TagList
  -> m ()

pixelAppendTags uuid tags = do
  backend <- view configConnection
  void . runCommand backend uuid $ addTags tags


-- Remove tags
pixelRemoveTags
  :: MonadIO m
  => MonadReader Config m
  => UUID
  -> TagList
  -> m ()

pixelRemoveTags uuid tags = do
  backend <- view configConnection
  void . runCommand backend uuid $ removeTags tags
