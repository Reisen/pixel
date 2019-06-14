module API.Image.Commands
  ( addTags
  , createImage
  , removeTags
  ) where

import Protolude
import Pixel.Lens
import Control.Lens
import Data.UUID          ( UUID )
import Eventless          ( Command, emit, loadSnapshot )
import Pixel.Model.Images ( Image(..), ImageEvent(..) )

--------------------------------------------------------------------------------

-- Useful Type Aliases
type Tag = Text


-- Create a new image altogether, this can also be used to populate the image
-- with some existing tags as well.
createImage
  :: Monad m
  => UUID
  -> Image
  -> Command Image m

createImage userUUID Image {..} = do
  emit (AssociatedWithUser userUUID)
  emit (HashChanged _hash)
  traverse_ (emit . TagAppended) _tags
  traverse_ (emit . CreatedAt) _createdAt

--------------------------------------------------------------------------------

-- Adds a tag to the set of tags for an image, only if it doesn't already have
-- it in the set.
addTags
  :: Monad m
  => Traversable t
  => t Tag
  -> Command Image m

addTags newtags = loadSnapshot @(Maybe Image) >>= \case
  Nothing  -> pure ()
  Just img -> for_ newtags $ \tag ->
    unless (elem tag $ img ^. tags)
      $ emit (TagAppended tag)

--------------------------------------------------------------------------------

-- Remove a tag from an images tag set, this will only succeed if the image
-- actually has the tag.
removeTags
  :: Monad m
  => Traversable t
  => t Tag
  -> Command Image m

removeTags newtags = loadSnapshot @(Maybe Image) >>= \case
  Nothing  -> pure ()
  Just img -> for_ newtags $ \tag ->
    when (elem tag $ img ^. tags)
      $ emit (TagRemoved tag)
