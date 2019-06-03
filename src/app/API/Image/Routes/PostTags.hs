module API.Image.Routes.PostTags
  ( PostTags
  , PostTagsRequest
  , postTags
  )
where

import Protolude
import Control.Lens
import Servant
import Data.Aeson         ( FromJSON (..) )
import MonadPixel         ( Pixel )
import Pixel              ( Error(..), pixelParseJSON )
import Pixel.Model.Images ( DigestText, ImageError(..), TagList, addTags )
import Pixel.Model.Token  ( Token )

--------------------------------------------------------------------------------
-- Post tags require:
--
--   * A UUID so we know which image we're talking about.
--   * A "tags" fragment to recognize /:uuid/tags routes.
--   * A Request Tags Object to decode from.

type PostTags =
  Header "Authorization" Token
    :> Capture "uuid" Text
    :> "tags"
    :> ReqBody '[JSON] PostTagsRequest
    :> Post '[JSON] NoContent

--------------------------------------------------------------------------------

newtype PostTagsRequest = PostTagsRequest
  { postTagsRequestTags :: TagList -- ^ List of tags to add to the image.
  } deriving (Show, Generic)

instance FromJSON PostTagsRequest where
  parseJSON = pixelParseJSON

makeFields ''PostTagsRequest

--------------------------------------------------------------------------------

postTags
  :: Maybe Token
  -> DigestText
  -> PostTagsRequest
  -> Pixel NoContent

-- When there's no Token, we can't do anything.
postTags Nothing _ _       = throwError (ImageError MissingToken)

-- When we have a token, authorize it and apply tags to the image object.
postTags (Just _) uuid req = do
  addTags uuid (req ^. tags)
  pure NoContent
