module API.Image.Commands
  ( addTags
  , createImage
  , removeTags
  )
where

import           Protolude               hiding ( hash )
import           API.Image.Types                ( Image(..), ImageEvent(..), HasTags(..), HasHash(..) )
import           Control.Lens
import           Data.UUID                      ( UUID )
import           Eventless                      ( Command
                                                , loadSnapshot
                                                , emit
                                                )

--------------------------------------------------------------------------------

-- Type Aliases
--
-- These don't do anything useful it just makes function types a bit more self
-- documenting / easier to read.
type Tag = Text


-- Create a new image altogether, this can also be used to populate the image
-- with some existing tags as well.
createImage
  :: Monad m
  => UUID
  -> Image
  -> Command Image m

createImage userUUID image = do
  emit (AssociatedWithUser userUUID)
  emit (HashChanged $ image ^. hash)
  traverse_ (emit . TagAppended) (image ^. tags)


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


-- Remove a tag from an images tag set, this will only succeed if the image
-- actually has the tag.
removeTags
  :: Monad m
  => Traversable t
  => t Tag
  -> Command Image m

removeTags newtags =
  loadSnapshot @(Maybe Image) >>= \case
    Nothing    -> pure ()
    Just image -> for_ newtags $ \tag ->
      when (elem tag $ image ^. tags)
        $ emit (TagRemoved tag)
