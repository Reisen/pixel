module API.Image.Routes.PostTags
  ( PostTags
  , PostTagsRequest
  , postTags
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens
import           Servant

import qualified Data.Aeson                    as A
import qualified Pixel                         as Pixel
import qualified MonadPixel                    as C

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type PostTags =
  Header "Authorization" Pixel.Token
    :> Capture "uuid" Text
    :> "tags"
    :> ReqBody '[JSON] PostTagsRequest
    :> Post '[JSON] NoContent

--------------------------------------------------------------------------------

newtype PostTagsRequest = PostTagsRequest
  { postTagsRequestTags :: Pixel.TagList
  } deriving (Show, Generic)

instance A.FromJSON PostTagsRequest where
  parseJSON = Pixel.pixelParseJSON

makeFields ''PostTagsRequest

--------------------------------------------------------------------------------

postTags
  :: Maybe Pixel.Token
  -> Pixel.DigestText
  -> PostTagsRequest
  -> C.Pixel NoContent

postTags Nothing _ _ = throwError (Pixel.ImageError Pixel.MissingToken)
postTags (Just _) uuid req = do
  Pixel.addTags uuid (req ^. tags)
  pure NoContent
