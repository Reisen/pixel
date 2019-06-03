module API.Image.Routes.GetTags
  ( GetTags
  , GetTagsResponse
  , getTags
  )
where

import Protolude
import Servant
import Data.Aeson         ( ToJSON(..) )
import Pixel              ( Error(..), pixelToEncoding, pixelToJSON )
import Pixel.Model.Token  ( Token )
import Pixel.Model.Images ( DigestText, TagList, ImageError(..), fetchTags )
import MonadPixel         ( Pixel )

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type GetTags =
  Header "Authorization" Token
    :> Capture "uuid" Text
    :> "tags"
    :> Get '[JSON] GetTagsResponse

--------------------------------------------------------------------------------

newtype GetTagsResponse = GetTagsResponse
  { getTagsResponseTags :: TagList
  } deriving (Show, Generic)

instance ToJSON GetTagsResponse where
  toEncoding = pixelToEncoding
  toJSON     = pixelToJSON

--------------------------------------------------------------------------------

getTags
  :: Maybe Token
  -> DigestText
  -> Pixel GetTagsResponse

getTags Nothing _     = throwError (ImageError MissingToken)
getTags (Just _) uuid =
  fetchTags uuid >>= \case
    Nothing       -> throwError (ImageError InvalidUUID)
    Just response -> pure (GetTagsResponse response)
