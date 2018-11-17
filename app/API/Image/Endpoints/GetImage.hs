module API.Image.Endpoints.GetImage
  ( GetImage
  , ImageResponse(..)
  , GetImageResponse(..)
  )
where

import           Protolude
import           Control.Lens
import           Data.Aeson                     ( FromJSON, ToJSON )
import           Servant
import           System.IO                      ( FilePath )


-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type GetImage = Get '[JSON] [GetImageResponse]


-- Define a type to wrap up the MultipartData coming over the wire.
data ImageResponse = ImageResponse
  { imageResponseUUID :: Text
  , imageResponsePath :: FilePath
  , imageResponseTags :: [Text]
  , imageResponseDate :: Text
  }
  deriving (Show, Generic)


-- Return a list of matching images for the request.
data GetImageResponse = GetImageResponse
  { getImageResponseImages :: [ImageResponse]
  }
  deriving (Show, Generic)


-- Generate Lenses
makeFields ''ImageResponse
makeFields ''GetImageResponse


-- Generate JSON
instance ToJSON ImageResponse where
instance FromJSON ImageResponse where
instance ToJSON GetImageResponse where
instance FromJSON GetImageResponse where
