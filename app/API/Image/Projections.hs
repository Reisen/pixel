module API.Image.Projections
  ( projectImages
  )
where

--------------------------------------------------------------------------------

import           Protolude

import qualified API.Image.Types               as API
import qualified Data.UUID                     as U
import qualified Database.SQLite.Simple        as S

--------------------------------------------------------------------------------

projectImages
  :: MonadIO m
  => S.Connection
  -> U.UUID
  -> API.Image
  -> m ()

projectImages conn uuid API.Image {..} = do
  putText $ fold ["[P:Image] ", show _imageCreatedAt, " ", _imageHash, ", Tags: ", show _imageTags]
  liftIO $ S.withTransaction conn $ do
    -- Create Tables
    S.execute_ conn createImageTable
    S.execute_ conn createTagsTable
    S.execute_ conn createTagAssociationTable

    -- Insert Image Updates
    S.execute conn insertImageRow (U.toText uuid, _imageHash, _imageCreatedAt)
    for_ _imageTags $ \tag -> do
      S.execute conn insertTagRow (tag, "tag" :: Text)
      S.execute conn insertAssociationRow (tag, U.toText uuid)

--------------------------------------------------------------------------------

createImageTable :: S.Query
createImageTable = "\
\ CREATE TABLE IF NOT EXISTS images ( \
\   uuid       TEXT     NOT NULL,     \
\   hash       TEXT     NOT NULL,     \
\   created    DATETIME NOT NULL,     \
\   PRIMARY KEY (uuid, hash)          \
\ );"

createTagsTable :: S.Query
createTagsTable = "\
\ CREATE TABLE IF NOT EXISTS tags ( \
\   name       TEXT     NOT NULL,   \
\   kind       TEXT     NOT NULL,   \
\   PRIMARY KEY (name, kind)        \
\ );"

createTagAssociationTable :: S.Query
createTagAssociationTable = "\
\ CREATE TABLE IF NOT EXISTS tag_associations (  \
\   name       TEXT     NOT NULL,                \
\   image      TEXT     NOT NULL,                \
\   FOREIGN KEY (name)  REFERENCES tags(name),   \
\   FOREIGN KEY (image) REFERENCES images(uuid), \
\   PRIMARY KEY (name, image)                    \
\ );"

insertImageRow :: S.Query
insertImageRow = "\
\ REPLACE INTO images ( uuid , hash , created ) \
\ VALUES              ( ?    , ?    , ?       ) \
\ "

insertTagRow :: S.Query
insertTagRow = "\
\ INSERT INTO tags       ( name, kind ) \
\ VALUES                 ( ?   , ?    ) \
\ ON CONFLICT DO NOTHING                \
\ "

insertAssociationRow :: S.Query
insertAssociationRow = "\
\ INSERT INTO tag_associations (name, image) \
\ VALUES                       ( ?   , ?   ) \
\ ON CONFLICT DO NOTHING                     \
\ "
