module API.Image.Persist
  ( fetchImage
  , persistImage
  )
where

import           Protolude
import           API.Image.Types                ( Image )
import           API.Image.Commands             ( createImage )
import           Configuration                  ( HasConnection(..) )
import           Control.Lens
import           Data.UUID
import           Eventless                      ( BackendStore
                                                , loadLatest
                                                , runCommand
                                                , value
                                                )


-- We want to persist an image in the backend using our event sourcing backend.
-- To do this we use runCommand.
persistImage
  :: MonadIO m
  => MonadReader config m
  => HasConnection config BackendStore
  => UUID
  -> Image
  -> m ()

persistImage uuid _ =
  view connection >>= \backend ->
    runCommand backend uuid $ do
      let hash = ""
      let tags = []
      createImage hash "static/" tags


-- We always attempt to fetch the latest aggregate value when pulling an Image
-- from the backend.
fetchImage
  :: MonadIO m
  => MonadReader config m
  => HasConnection config BackendStore
  => UUID
  -> m (Maybe Image)

fetchImage uuid =
  view connection
    >>= flip loadLatest uuid
    >>= pure . map (^. value)
