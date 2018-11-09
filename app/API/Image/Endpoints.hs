module API.Image.Endpoints
  ( ReqImage
  , postImage
  , getImage
  , getImageByUUID
  )
where

import           Protolude
import           API.Image.Persist              ( persistImage )
import           Configuration                  ( MonadImageless, HasConnection )
import           Control.Lens
import           Data.Aeson                     ( FromJSON, ToJSON )
import           Data.Data                      ( Data )
import           Data.UUID                      ( nextRandom )
import           Eventless                      ( BackendStore )
import           Servant                        ( NoContent(..)
                                                , (:>)
                                                , Get
                                                , Post
                                                , JSON
                                                , Capture
                                                , ReqBody
                                                )


data ReqImage = ReqImage
  { hash :: Text
  , path :: Text
  , tags :: [Text]
  }
  deriving (Show, Generic, Typeable, Data)

instance FromJSON ReqImage
instance ToJSON ReqImage


postImage
  :: MonadImageless m
  => MonadReader config m
  => HasConnection config BackendStore
  => MultiPartBody
  -> ReqImage
  -> m NoContent

postImage multi image = do
  uuid <- liftIO nextRandom
  hash <- pure ""
  tags <- pure []
  _    <- persistImage uuid image
  pure NoContent


getImage
  :: MonadImageless m
  => m [ReqImage]

getImage = undefined


getImageByUUID
  :: MonadImageless m
  => Text
  -> m ReqImage

getImageByUUID = undefined

