module API.Image.Commands
  ( addTags
  , createImage
  , removeTags
  ) where

--------------------------------------------------------------------------------

import Protolude
import Control.Lens

import Data.UUID ( UUID )
import Eventless ( Command, emit, loadSnapshot )
import Pixel     ( Image (..)
                 , ImageEvent (..)
                 , imageTags
                 )

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
  emit (HashChanged _imageHash)
  traverse_ (emit . TagAppended) _imageTags
  traverse_ (emit . CreatedAt) _imageCreatedAt

--------------------------------------------------------------------------------

-- Adds a tag to the set of tags for an image, only if it doesn't already have
-- it in the set.
addTags
  :: Monad m
  => Traversable t
  => t Tag
  -> Command Image m

addTags newtags = loadSnapshot @(Maybe Image) >>= \case
  Nothing    -> pure ()
  Just image -> for_ newtags $ \tag ->
    unless (elem tag $ image ^. imageTags)
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
  Nothing    -> pure ()
  Just image -> for_ newtags $ \tag ->
    when (elem tag $ image ^. imageTags)
      $ emit (TagRemoved tag)
