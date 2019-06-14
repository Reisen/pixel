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
      Nothing  -> pure Nothing
      Just img -> pure . Just $ img ^. tags

--------------------------------------------------------------------------------

appendImageTags
  :: Monad m
  => MonadImage m
  => Text
  -> [Text]
  -> m ()

appendImageTags uuidText newTags =
  case fromText uuidText of
    Nothing      -> pure ()
    Just imgUUID -> appendTags imgUUID newTags

--------------------------------------------------------------------------------

removeImageTags
  :: Monad m
  => MonadImage m
  => Text
  -> [Text]
  -> m ()

removeImageTags uuidText newTags = case fromText uuidText of
  Nothing      -> pure ()
  Just imgUUID -> removeTags imgUUID newTags

