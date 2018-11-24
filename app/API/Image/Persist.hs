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
  => Traversable t
  => MonadReader config m
  => HasConnection config BackendStore
  => UUID
  -> FilePath
  -> t Text
  -> m UUID

persistImage userUUID filePath tags = do
  backend <- view connection
  uuid    <- liftIO nextRandom
  hashed  <- liftIO undefined
  runCommand backend uuid $ createImage hashed "static/" tags
  pure uuid


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
