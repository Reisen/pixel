module API.Image.Commands
  ( addTags
  , createImage
  , removeTags
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens

import qualified API.Image.Types               as API
import qualified Data.UUID                     as U
import qualified Eventless                     as E
import qualified Pixel                         as Pixel

--------------------------------------------------------------------------------

-- Useful Type Aliases
type Tag = Text


-- Create a new image altogether, this can also be used to populate the image
-- with some existing tags as well.
createImage
  :: Monad m
  => U.UUID
  -> Pixel.Image
  -> E.Command Pixel.Image m

createImage userUUID Pixel.Image {..} = do
  E.emit (API.AssociatedWithUser userUUID)
  E.emit (API.HashChanged _imageHash)
  traverse_ (E.emit . API.TagAppended) _imageTags
  traverse_ (E.emit . API.CreatedAt) _imageCreatedAt

--------------------------------------------------------------------------------

-- Adds a tag to the set of tags for an image, only if it doesn't already have
-- it in the set.
addTags
  :: Monad m
  => Traversable t
  => t Tag
  -> E.Command Pixel.Image m

addTags newtags = E.loadSnapshot @(Maybe Pixel.Image) >>= \case
  Nothing    -> pure ()
  Just image -> for_ newtags $ \tag ->
    unless (elem tag $ image ^. Pixel.imageTags)
      $ E.emit (API.TagAppended tag)

--------------------------------------------------------------------------------

-- Remove a tag from an images tag set, this will only succeed if the image
-- actually has the tag.
removeTags
  :: Monad m
  => Traversable t
  => t Tag
  -> E.Command Pixel.Image m

removeTags newtags = E.loadSnapshot @(Maybe Pixel.Image) >>= \case
  Nothing    -> pure ()
  Just image -> for_ newtags $ \tag ->
    when (elem tag $ image ^. Pixel.imageTags)
      $ E.emit (API.TagRemoved tag)
