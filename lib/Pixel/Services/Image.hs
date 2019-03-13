module Pixel.Services.Image
  ( MonadImage (..)
  )
where

--------------------------------------------------------------------------------

import           Protolude

import qualified Data.UUID                     as U
import qualified Pixel.API.Images.Types        as Pixel

--------------------------------------------------------------------------------

-- Tagless Final provider for methods that deal with metadata persistance of
-- images. The API must be storage agnostic.
class MonadImage m where
  -- Persist an image in the database, this assumes a brand new image each time
  -- and so always produces a new UUID for the recorded image.
  saveImage :: U.UUID -> Pixel.Image -> m ()

  -- Attempts to retrieve a saved image from somewhere.
  loadImage :: U.UUID -> m (Maybe Pixel.Image)

  -- Retrieve all images (lmited to a max) from backend.
  loadImages :: Int -> m [(U.UUID, Pixel.Image)]

  -- Append tags to a specific image UUID.
  appendTags :: U.UUID -> Pixel.TagList -> m ()

  -- Remove tags from a specific image UUID.
  removeTags :: U.UUID -> Pixel.TagList -> m ()
