module Pixel.API.Images.Operations.GetImageByUUID
  ( handleImageRequest
  ) where

import Protolude
import Data.UUID              ( fromText )
import Pixel.API.Images.Types ( DigestText, Image )
import Pixel.Services.Image   ( MonadImage (..) )

--------------------------------------------------------------------------------

handleImageRequest
  :: Monad m
  => MonadImage m
  => DigestText
  -> m (Maybe Image)

handleImageRequest textUUID =
  case fromText textUUID of
    Nothing   -> pure Nothing
    Just uuid -> loadImage uuid >>= \case
      Nothing    -> pure Nothing
      Just image -> pure (Just image)
