module Pixel.API.Images.Operations.GetImage
  ( fetchImages
  ) where

--------------------------------------------------------------------------------

import           Protolude

import qualified Data.UUID                     as U
import qualified Pixel.API.Images.Types        as Pixel
import qualified Pixel.Services.Image          as Pixel

--------------------------------------------------------------------------------

fetchImages
  :: Monad m
  => Pixel.MonadImage m
  => m [(U.UUID, Pixel.Image)]

fetchImages = Pixel.loadImages 10
