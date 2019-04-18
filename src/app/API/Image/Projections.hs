module API.Image.Projections
  ( projectImages
  ) where

--------------------------------------------------------------------------------

import Protolude

import Data.UUID              ( UUID, toText )
import Database.SQLite.Simple ( Query, Connection, execute_, execute, withTransaction )
import Pixel                  ( Image (..) )

--------------------------------------------------------------------------------

projectImages
  :: MonadIO m
  => Connection
  -> UUID
  -> Image
  -> m ()

projectImages conn uuid Image {..} = do
  putText $ fold ["[P:Image] ", show _imageCreatedAt, " ", _imageHash, ", Tags: ", show _imageTags]
  liftIO $ withTransaction conn $ do
    -- Create Tables
    execute_ conn createImageTable
    execute_ conn createTagsTable
    execute_ conn createTagAssociationTable

    -- Insert Image Updates
    execute conn insertImageRow (toText uuid, _imageHash, _imageCreatedAt)
    for_ _imageTags $ \tag -> do
      execute conn insertTagRow (tag, "tag" :: Text)
      execute conn insertAssociationRow (tag, toText uuid)

--------------------------------------------------------------------------------

createImageTable :: Query
createImageTable = "\
\ CREATE TABLE IF NOT EXISTS images ( \
\   uuid       TEXT     NOT NULL,     \
\   hash       TEXT     NOT NULL,     \
\   created    DATETIME NOT NULL,     \
\   PRIMARY KEY (uuid, hash)          \
\ );"

createTagsTable :: Query
createTagsTable = "\
\ CREATE TABLE IF NOT EXISTS tags ( \
\   name       TEXT     NOT NULL,   \
\   kind       TEXT     NOT NULL,   \
\   PRIMARY KEY (name, kind)        \
\ );"

createTagAssociationTable :: Query
createTagAssociationTable = "\
\ CREATE TABLE IF NOT EXISTS tag_associations (  \
\   name       TEXT     NOT NULL,                \
\   image      TEXT     NOT NULL,                \
\   FOREIGN KEY (name)  REFERENCES tags(name),   \
\   FOREIGN KEY (image) REFERENCES images(uuid), \
\   PRIMARY KEY (name, image)                    \
\ );"

insertImageRow :: Query
insertImageRow = "\
\ REPLACE INTO images ( uuid , hash , created ) \
\ VALUES              ( ?    , ?    , ?       ) \
\ "

insertTagRow :: Query
insertTagRow = "\
\ INSERT INTO tags       ( name, kind ) \
\ VALUES                 ( ?   , ?    ) \
\ ON CONFLICT DO NOTHING                \
\ "

insertAssociationRow :: Query
insertAssociationRow = "\
\ INSERT INTO tag_associations (name, image) \
\ VALUES                       ( ?   , ?   ) \
\ ON CONFLICT DO NOTHING                     \
\ "
