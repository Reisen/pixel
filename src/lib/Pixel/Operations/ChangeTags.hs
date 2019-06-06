module Pixel.Operations.ChangeTags
  ( appendImageTags
  , findTagsByUUID
  , removeImageTags
  ) where

import Protolude
import Pixel.Lens
import Control.Lens
import Data.UUID            ( fromText )
import Pixel.Services.Image ( MonadImage(..) )

--------------------------------------------------------------------------------

findTagsByUUID
  :: Monad m
  => MonadImage m
  => Text
  -> m (Maybe [Text])

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
  => Text
  -> [Text]
  -> m ()

appendImageTags uuidText newTags =
  case fromText uuidText of
    Nothing        -> pure ()
    Just imageUUID -> appendTags imageUUID newTags

--------------------------------------------------------------------------------

removeImageTags
  :: Monad m
  => MonadImage m
  => Text
  -> [Text]
  -> m ()

removeImageTags uuidText newTags = case fromText uuidText of
  Nothing        -> pure ()
  Just imageUUID -> removeTags imageUUID newTags

