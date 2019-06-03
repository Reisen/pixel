module Pixel.Services.Image
  ( MonadImage (..)
  )
where

--------------------------------------------------------------------------------

import Protolude
import Data.UUID                ( UUID )
import Pixel.Model.Images.Types ( Image, TagList )

--------------------------------------------------------------------------------

-- Tagless Final provider for methods that deal with metadata persistance of
-- images. The API must be storage agnostic.
class MonadImage m where
  -- Persist an image in the database, this assumes a brand new image each time
  -- and so always produces a new UUID for the recorded image.
  saveImage :: UUID -> Image -> m ()

  -- Attempts to retrieve a saved image from somewhere.
  loadImage :: UUID -> m (Maybe Image)

  -- Retrieve all images (lmited to a max) from backend.
  loadImages :: Int -> m [(UUID, Image)]

  -- Append tags to a specific image UUID.
  appendTags :: UUID -> TagList -> m ()

  -- Remove tags from a specific image UUID.
  removeTags :: UUID -> TagList -> m ()
