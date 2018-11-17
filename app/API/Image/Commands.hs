module API.Image.Commands
  ( addTags
  , createImage
  , removeTags
  )
where

import           Protolude
import           API.Image.Types
import           Control.Lens
import           Eventless                      ( Command, loadSnapshot, emit )


-- Type Aliases
--
-- These don't do anything useful it just makes function types a bit more self
-- documenting / easier to read.
type Hash = Text
type Path = Text
type Tag  = Text


-- Adds a tag to the set of tags for an image, only if it doesn't already have
-- it in the set.
addTags
  :: Monad m
  => Traversable t
  => t Tag
  -> Command Image m

addTags newtags =
  loadSnapshot @(Maybe Image) >>= \case
    Nothing    -> pure ()
    Just image -> for_ newtags $ \tag ->
      unless (elem tag $ image ^. tags)
        $ emit (TagAppended tag)


-- Create a new image altogether, this can also be used to populate the image
-- with some existing tags as well.
createImage
  :: Monad m
  => Traversable t
  => Hash
  -> Path
  -> t Tag
  -> Command Image m

createImage hash path tags = do
  emit (HashChanged hash)
  emit (PathChanged $ fold ["/", hash, ".png"])
  for_ tags (emit . TagAppended)


-- Remove a tag from an images tag set, this will only succeed if the image
-- actually has the tag.
removeTags
  :: Monad m
  => Traversable t
  => t Tag
  -> Command Image m

removeTags newtags = do
  loadSnapshot @(Maybe Image) >>= \case
    Nothing    -> pure ()
    Just image -> for_ newtags $ \tag ->
      when (elem tag $ image ^. tags)
        $ emit (TagRemoved tag)
