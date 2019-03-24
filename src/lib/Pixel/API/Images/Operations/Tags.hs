module Pixel.API.Images.Operations.Tags
  ( fetchTags
  , addTags
  , deleteTags
  ) where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens

import qualified Data.UUID                     as U
import qualified Pixel.API.Images.Types        as Pixel
import qualified Pixel.Services.Image          as Pixel

--------------------------------------------------------------------------------

fetchTags
  :: Monad m
  => Pixel.MonadImage m
  => Pixel.DigestText
  -> m (Maybe Pixel.TagList)

fetchTags uuidText =
  case U.fromText uuidText of
    Nothing   -> pure Nothing
    Just uuid -> Pixel.loadImage uuid >>= \case
      Nothing    -> pure Nothing
      Just image -> pure . Just $ image ^. Pixel.imageTags

--------------------------------------------------------------------------------

addTags
  :: Monad m
  => Pixel.MonadImage m
  => Pixel.DigestText
  -> Pixel.TagList
  -> m ()

addTags uuidText newTags =
  case U.fromText uuidText of
    Nothing   -> pure ()
    Just uuid -> Pixel.appendTags uuid newTags

--------------------------------------------------------------------------------

deleteTags
  :: Monad m
  => Pixel.MonadImage m
  => Pixel.DigestText
  -> Pixel.TagList
  -> m ()

deleteTags uuidText newTags = case U.fromText uuidText of
  Nothing   -> pure ()
  Just uuid -> Pixel.removeTags uuid newTags
