module API.Image.Persist
  ( fetchImage
  , persistImage
  )
where

import           Protolude               hiding ( to )
import           API.Image.Types                ( Image, HasUploader(..) )
import           API.Image.Commands             ( createImage )
import           Configuration                  ( HasConnection(..) )
import           Control.Lens
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )
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
  => Image
  -> m ()

persistImage image = case (image ^. uploader) of
  Nothing       -> undefined
  Just userUUID -> do
    backend <- view connection
    uuid    <- liftIO nextRandom
    void . runCommand backend uuid $ createImage userUUID image


-- We always attempt to fetch the latest aggregate value when pulling an Image
-- from the backend.
fetchImage
  :: MonadIO m
  => MonadReader config m
  => HasConnection config BackendStore
  => UUID
  -> m (Maybe Image)

fetchImage uuid =
  map (^. value) <$> (view connection >>= flip loadLatest uuid)
