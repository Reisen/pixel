module Pixel.Model.Images.Operations.GetImage
  ( fetchImages
  ) where

import Protolude
import Data.UUID                ( UUID )
import Pixel.Model.Images.Types ( Image )
import Pixel.Services.Image     ( MonadImage (..) )

--------------------------------------------------------------------------------

fetchImages
  :: Monad m
  => MonadImage m
  => m [(UUID, Image)]

fetchImages = loadImages 10
