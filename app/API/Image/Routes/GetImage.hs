module API.Image.Routes.GetImage
  ( GetImage
  , GetImageResponse(..)
  , ImageMetadata(..)
  , ImageResponse(..)
  , ImageTag(..)
  , getImage
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens
import           Servant

import qualified API.Image.Services            as API
import qualified API.Image.Types               as API
import qualified API.Token                     as API
import qualified Configuration                 as C
import qualified Data.Aeson                    as A

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract
-- away from the backend.
type GetImage =
  Header "Authorization" API.Token
    :> Get '[JSON] GetImageResponse

--------------------------------------------------------------------------------

-- Tagging Data
data ImageTag = ImageTag
  { imageTagKind :: Text
  , imageTagName :: Text
  } deriving (Show, Generic)

instance A.ToJSON ImageTag where

--------------------------------------------------------------------------------

data ImageMetadata = ImageMetadata
  { imageMetadataPath  :: Text
  , imageMetadataThumb :: Text
  } deriving (Show, Generic)

instance A.ToJSON ImageMetadata where

--------------------------------------------------------------------------------

-- What an Image is rendered as in the response.
data ImageResponse = ImageResponse
  { imageResponseUUID :: Text
  , imageResponseHash :: Text
  , imageResponseTags :: [ImageTag]
  , imageResponseDate :: Text
  , imageResponseMeta :: ImageMetadata
  } deriving (Show, Generic)

instance A.ToJSON ImageResponse where

--------------------------------------------------------------------------------

-- Return a list of matching images for the request.
newtype GetImageResponse = GetImageResponse
  { getImageResponseImages :: [ImageResponse]
  } deriving (Show, Generic)

instance A.ToJSON GetImageResponse where

--------------------------------------------------------------------------------

-- Fetch images from the backend, this endpoint accepts a filter to decide what
-- to return from the backend. By default this is all images ordered by upload
-- date and the filter should encompass all possible queries a user might have,
-- from tags to uploader, to ordering.
getImage
  :: C.MonadPixel m
  => Maybe API.Token
  -> m GetImageResponse

getImage _token = do
  images <- undefined
  pure GetImageResponse
    { getImageResponseImages = flip map images $ \image ->
        ImageResponse
          { imageResponseUUID = undefined
          , imageResponseHash = image ^. API.imageHash
          , imageResponseTags = map (ImageTag "tag") (image ^. API.imageTags)
          , imageResponseDate = undefined
          , imageResponseMeta = ImageMetadata
              { imageMetadataPath  = makeImagePath image
              , imageMetadataThumb = makeImageThumb image
              }
          }
    }

  where
    -- Build Canonical Paths
    makeImagePath image  = fold [ "/static/images/" , image ^. API.imageHash , ".png" ]
    makeImageThumb image = fold [ "/static/thumbs/" , image ^. API.imageHash , ".png" ]
