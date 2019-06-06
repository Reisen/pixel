module Pixel.Operations.FindImages
  ( findImages
  ) where

import Protolude
import Data.UUID            ( UUID )
import Pixel.Model.Images   ( Image )
import Pixel.Services.Image ( MonadImage (..) )

--------------------------------------------------------------------------------

findImages
  :: Monad m
  => MonadImage m
  => m [(UUID, Image)]

findImages = loadImages 10
