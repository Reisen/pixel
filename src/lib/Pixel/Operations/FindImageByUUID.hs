module Pixel.Operations.FindImageByUUID
  ( findImageByUUID
  ) where

import Protolude
import Data.UUID            ( fromText )
import Pixel.Model.Images   ( Image )
import Pixel.Services.Image ( MonadImage (..) )

--------------------------------------------------------------------------------

findImageByUUID
  :: Monad m
  => MonadImage m
  => Text
  -> m (Maybe Image)

findImageByUUID textUUID = case fromText textUUID of
  Nothing   -> pure Nothing
  Just uuid -> loadImage uuid >>= \case
    Nothing    -> pure Nothing
    Just image -> pure (Just image)

