module API.Image.Endpoints
  ( ReqImage
  , PostImage
  , postImage
  , GetImage
  , getImage
  , GetImageByUUID
  , getImageByUUID
  ) where

import Protolude
import API.Image.Persist (persistImage)
import Configuration     (MonadImageless, HasConnection)
import Control.Lens
import Data.Aeson        (FromJSON, ToJSON)
import Data.Data         (Data)
import Eventless         (BackendStore)
import Servant
  ( NoContent(..)
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


type PostImage
  =  MultiPartBody
  :> ReqBody '[JSON] ReqImage
  :> Post '[JSON] NoContent

postImage
  :: MonadImageless m
  => MonadReader config m
  => HasConnection config BackendStore
  => MultiPartBody
  -> ReqImage
  -> m NoContent

postImage multi image = do
  hash <- calculateFileHash multi
  tags <- getTagsFrom multi
  uuid <- generateUUID
  _    <- persistImage uuid image
  pure NoContent


type GetImage = Get '[JSON] [ReqImage]

getImage
  :: MonadImageless m
  => m [ReqImage]

getImage = undefined


type GetImageByUUID
  =  Capture "uuid" Text
  :> Get '[JSON] ReqImage

getImageByUUID
  :: MonadImageless m
  => Text
  -> m ReqImage

getImageByUUID = undefined

