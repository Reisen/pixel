module Pixel.API.Images.Operations.GetImageByUUID
  ( handleImageRequest
  ) where

--------------------------------------------------------------------------------

import           Protolude

import qualified Data.UUID                     as U
import qualified Pixel.API.Images.Types        as Pixel
import qualified Pixel.Services.Image          as Pixel

--------------------------------------------------------------------------------

handleImageRequest
  :: Monad m
  => Pixel.MonadImage m
  => Pixel.DigestText
  -> m (Maybe Pixel.Image)

handleImageRequest uuidText =
  case U.fromText uuidText of
    Nothing   -> pure Nothing
    Just uuid -> Pixel.loadImage uuid >>= \case
      Nothing    -> pure Nothing
      Just image -> pure (Just image)
