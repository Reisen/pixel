module Services.Image
  ( pixelSaveImage
  , pixelLoadImage
  , pixelLoadImages
  , pixelAppendTags
  , pixelRemoveTags
  ) where

--------------------------------------------------------------------------------

import Protolude
import Pixel.Lens
import Control.Lens
import Text.InterpolatedString.QM
import API.Image.Commands         ( createImage, addTags, removeTags )
import Configuration              ( Config, configConnection, configReadSchema )
import Data.Text                  ( splitOn )
import Data.UUID                  ( UUID, fromText )
import Database.SQLite.Simple     ( query_ )
import Eventless                  ( runCommand, loadLatest, value )
import Pixel.Model.Images         ( Image(..), TagList )

--------------------------------------------------------------------------------

pixelSaveImage
  :: MonadIO m
  => MonadReader Config m
  => UUID
  -> Image
  -> m ()

pixelSaveImage uuid image = case image ^. uploader of
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
  schema    <- view configReadSchema
  imageRows <- liftIO $ query_ schema [qns|
      SELECT    i.uuid
              , i.hash
              , i.created
              , group_concat(ta.name) tags
      FROM      images i
      LEFT JOIN tag_associations ta ON i.uuid = ta.image
      GROUP BY  i.uuid
      ORDER BY  i.created DESC
  |]

  pure . catMaybes $ imageRows <&> \(textUUID, imageHash, date, imageTags) ->
    case fromText textUUID of
      Nothing   -> Nothing
      Just uuid -> Just
        ( uuid
        , Image
          { _hash      = imageHash
          , _tags      = fromMaybe [] $ splitOn "," <$> imageTags
          , _uploader  = Nothing
          , _createdAt = Just date
          , _deletedAt = Nothing
          }
        )


pixelAppendTags
  :: MonadIO m
  => MonadReader Config m
  => UUID
  -> TagList
  -> m ()

pixelAppendTags uuid imageTags = do
  backend <- view configConnection
  void . runCommand backend uuid $ addTags imageTags


-- Remove tags
pixelRemoveTags
  :: MonadIO m
  => MonadReader Config m
  => UUID
  -> TagList
  -> m ()

pixelRemoveTags uuid imageTags = do
  backend <- view configConnection
  void . runCommand backend uuid $ removeTags imageTags
