module Pixel.Model.Images.Operations.Tags
  ( fetchTags
  , addTags
  , deleteTags
  ) where

import Protolude
import Pixel.Lens
import Control.Lens
import Data.UUID                ( fromText )
import Pixel.Model.Images.Types ( TagList, DigestText )
import Pixel.Services.Image     ( MonadImage(..) )

--------------------------------------------------------------------------------

fetchTags
  :: Monad m
  => MonadImage m
  => DigestText
  -> m (Maybe TagList)

fetchTags uuidText =
  case fromText uuidText of
    Nothing   -> pure Nothing
    Just uuid -> loadImage uuid >>= \case
      Nothing    -> pure Nothing
      Just image -> pure . Just $ image ^. tags

--------------------------------------------------------------------------------

addTags
  :: Monad m
  => MonadImage m
  => DigestText
  -> TagList
  -> m ()

addTags uuidText newTags =
  case fromText uuidText of
    Nothing   -> pure ()
    Just uuid -> appendTags uuid newTags

--------------------------------------------------------------------------------

deleteTags
  :: Monad m
  => MonadImage m
  => DigestText
  -> TagList
  -> m ()

deleteTags uuidText newTags = case fromText uuidText of
  Nothing   -> pure ()
  Just uuid -> removeTags uuid newTags
