module API.Image.Persist
  ( fetchImage
  , persistImage
  ) where

import Protolude
import API.Image.Types    (Image)
import API.Image.Commands (createImage)
import Configuration      (HasConnection (..))
import Control.Lens
import Eventless
  ( BackendStore
  , UUID
  , loadAggregate
  , runCommand
  , value
  )


persistImage
  :: MonadIO m
  => MonadReader config m
  => HasConnection config BackendStore
  => UUID
  -> Image
  -> m ()

-- We want to persist an image in the backend using our event sourcing
-- backend. To do this we use runCommand.
persistImage uuid _ =
  view connection >>= \backend ->
    runCommand backend uuid $ do
      let hash = ""
      let tags = []
      createImage hash "static/" tags


fetchImage
  :: MonadIO m
  => MonadReader config m
  => HasConnection config BackendStore
  => UUID
  -> m (Maybe Image)

fetchImage uuid =
      view connection
  >>= flip loadAggregate uuid
  >>= pure . map value
