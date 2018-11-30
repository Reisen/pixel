module API.Image.Endpoints.GetImageByUUID
  ( GetImageByUUID
  , GetImageByUUIDResponse(..)
  , HasImage(..)
  )
where

import           Protolude
import           API.Image.Endpoints.GetImage   ( ImageResponse )
import           Control.Lens
import           Data.Aeson                     ( FromJSON, ToJSON )
import           Servant


-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type GetImageByUUID
  =  Capture "uuid" Text
  :> Get '[JSON] GetImageByUUIDResponse


-- Define a type to wrap up the MultipartData coming over the wire.
newtype GetImageByUUIDResponse = GetImageByUUIDResponse
  { getImageByUUIDResponseImage :: ImageResponse
  } deriving (Show, Generic)


-- Generate Lenses & JSON
makeFields ''GetImageByUUIDResponse
instance ToJSON GetImageByUUIDResponse where
instance FromJSON GetImageByUUIDResponse where
