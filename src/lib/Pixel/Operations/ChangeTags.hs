module Pixel.Operations.ChangeTags
  ( appendImageTags
  , findTagsByUUID
  , removeImageTags
  ) where

import Protolude
import Pixel.Lens
import Control.Lens
import Data.UUID                ( fromText )
import Pixel.Model.Images.Types ( TagList, DigestText )
import Pixel.Services.Image     ( MonadImage(..) )

--------------------------------------------------------------------------------

findTagsByUUID
  :: Monad m
  => MonadImage m
  => DigestText
  -> m (Maybe TagList)

findTagsByUUID uuidText =
  case fromText uuidText of
    Nothing        -> pure Nothing
    Just imageUUID -> loadImage imageUUID >>= \case
      Nothing    -> pure Nothing
      Just image -> pure . Just $ image ^. tags

--------------------------------------------------------------------------------

appendImageTags
  :: Monad m
  => MonadImage m
  => DigestText
  -> TagList
  -> m ()

appendImageTags uuidText newTags =
  case fromText uuidText of
    Nothing        -> pure ()
    Just imageUUID -> appendTags imageUUID newTags

--------------------------------------------------------------------------------

removeImageTags
  :: Monad m
  => MonadImage m
  => DigestText
  -> TagList
  -> m ()

removeImageTags uuidText newTags = case fromText uuidText of
  Nothing        -> pure ()
  Just imageUUID -> removeTags imageUUID newTags

