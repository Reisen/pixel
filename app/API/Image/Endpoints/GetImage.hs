module API.Image.Endpoints.GetImage
  ( GetImage
  , ImageResponse(..)
  , GetImageResponse(..)
  )
where

import           Protolude
import           Control.Lens
import           Data.Aeson                     ( ToJSON )
import           Servant
import           System.IO                      ( FilePath )

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type GetImage = Get '[JSON] [GetImageResponse]

--------------------------------------------------------------------------------

-- Tagging Data
data ImageTag = ImageTag
  { imageTagKind :: Text
  , imageTagName :: Text
  } deriving (Show, Generic)

instance ToJSON ImageTag where

--------------------------------------------------------------------------------

-- Define a type to wrap up the MultipartData coming over the wire.
data ImageResponse = ImageResponse
  { imageResponseUUID :: Text
  , imageResponseHash :: Text
  , imageResponseTags :: [ImageTag]
  , imageResponseDate :: Text
  } deriving (Show, Generic)

instance ToJSON ImageResponse where

--------------------------------------------------------------------------------

-- Return a list of matching images for the request.
newtype GetImageResponse = GetImageResponse
  { getImageResponseImages :: [ImageResponse]
  } deriving (Show, Generic)

instance ToJSON GetImageResponse where

--------------------------------------------------------------------------------

-- Generate Lenses
makeFields ''ImageTag
makeFields ''ImageResponse
makeFields ''GetImageResponse
