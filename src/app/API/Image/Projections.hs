module API.Image.Projections
  ( projectImages
  , setupImageProjections
  ) where

import Protolude
import Text.InterpolatedString.QM

import Data.UUID              ( UUID, toText )
import Database.SQLite.Simple ( Query
                              , Connection
                              , Only (..)
                              , execute_
                              , execute
                              , withTransaction
                              )
import Pixel.API.Images       ( Image (..) )

--------------------------------------------------------------------------------

-- Ran on first run in order to setup table schema for projection, this also
-- writes any non-projected (but required data), such as standard roles. As
-- they are not event backed, they always exist and survive deletion after a
-- restart.
setupImageProjections
  :: MonadIO m
  => Connection
  -> m ()

setupImageProjections conn = liftIO $ withTransaction conn $ do
  -- Create Core Tables
  execute_ conn createImageTable
  execute_ conn createTagsTable
  execute_ conn createTagAssociationTable


projectImages
  :: MonadIO m
  => Connection
  -> UUID
  -> Image
  -> m ()

projectImages conn uuid Image {..} = liftIO $ withTransaction conn $ do
  putText $ fold ["[P:Image] ", show _imageCreatedAt, " ", _imageHash, ", Tags: ", show _imageTags]

  -- Insert Image Updates
  execute conn upsertImageRow (toText uuid, _imageHash, _imageCreatedAt)

  -- Recreate Tag Mapping
  execute conn clearAssociations (Only $ toText uuid)

  for_ _imageTags $ \tag -> do
    execute conn insertTagRow (tag, "tag" :: Text)
    execute conn insertAssociationRow (tag, toText uuid)

--------------------------------------------------------------------------------

createImageTable :: Query
createImageTable = [qns|
  CREATE TABLE IF NOT EXISTS images (
    uuid       TEXT     NOT NULL,
    hash       TEXT     NOT NULL,
    created    DATETIME NOT NULL,
    PRIMARY KEY (uuid, hash)
  );
|]

createTagsTable :: Query
createTagsTable = [qns|
  CREATE TABLE IF NOT EXISTS tags (
    name       TEXT     NOT NULL,
    kind       TEXT     NOT NULL,
    PRIMARY KEY (name, kind)
  );
|]

createTagAssociationTable :: Query
createTagAssociationTable = [qns|
  CREATE TABLE IF NOT EXISTS tag_associations (
    name       TEXT     NOT NULL,
    image      TEXT     NOT NULL,
    FOREIGN KEY (name)  REFERENCES tags(name),
    FOREIGN KEY (image) REFERENCES images(uuid),
    PRIMARY KEY (name, image)
  );
|]

upsertImageRow :: Query
upsertImageRow = [qns|
  REPLACE INTO images ( uuid , hash , created )
  VALUES              ( ?    , ?    , ?       );
|]

insertTagRow :: Query
insertTagRow = [qns|
  INSERT INTO tags       ( name, kind )
  VALUES                 ( ?   , ?    )
  ON CONFLICT DO NOTHING;
|]

insertAssociationRow :: Query
insertAssociationRow = [qns|
  INSERT INTO tag_associations (name, image)
  VALUES                       ( ?   , ?   )
  ON CONFLICT DO NOTHING;
|]

clearAssociations :: Query
clearAssociations = [qns|
  DELETE FROM tag_associations
  WHERE       image = ?
|]
