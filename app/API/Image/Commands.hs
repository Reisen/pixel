module API.Image.Commands
  ( addTag
  , createImage
  ) where

import Protolude
import API.Image.Types
import Control.Lens
import Eventless       (Command, loadSnapshot, emit)


-- Type Aliases
type Hash = Text
type Path = Text
type Tag  = Text


addTag
  :: Monad m
  => Tag
  -> Command Image m

addTag tag =
  loadSnapshot @(Maybe Image) >>= \case
    Nothing    -> pure ()
    Just image -> unless (elem tag $ image ^. tags)
      $ emit (TagAppended tag)


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


removeTag
  :: Monad m
  => Tag
  -> Command Image m

removeTag tag = do
  loadSnapshot @(Maybe Image) >>= \case
    Nothing    -> pure ()
    Just image -> when (elem tag $ image ^. tags)
      $ emit (TagRemoved tag)
