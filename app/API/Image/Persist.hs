module API.Image.Persist
  ( fetchImage
  , PersistImage(..)
  , persistImage
  )
where

import           Protolude
import           API.Token                      ( Token(..) )
import           API.Image.Types                ( Image )
import           API.Image.Commands             ( createImage )
import           Configuration                  ( HasConnection(..) )
import           Control.Lens
import           Data.UUID                      ( UUID, fromText )
import           Data.UUID.V4                   ( nextRandom )
import           Eventless                      ( BackendStore
                                                , loadLatest
                                                , runCommand
                                                , value
                                                )

data PersistImage = PersistImage
  { _piUser :: Token
  , _piHash :: Text
  , _piTags :: [Text]
  } deriving (Show)


-- We want to persist an image in the backend using our event
-- sourcing backend. To do this we use runCommand.
persistImage
  :: MonadIO m
  => MonadReader config m
  => HasConnection config BackendStore
  => PersistImage
  -> m ()

persistImage image =
  case image & fromText . tokenText . _piUser of
    Nothing       -> undefined
    Just userUUID -> do
      backend <- view connection
      uuid    <- liftIO nextRandom
      void $ runCommand backend uuid $
        createImage
          userUUID
          (_piHash image)
          (_piTags image)


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
