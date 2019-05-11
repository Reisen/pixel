module API.Image.Routes.DeleteTags
  ( DeleteTags
  , DeleteTagsRequest
  , postDeleteTags
  ) where

--------------------------------------------------------------------------------

import Protolude
import Control.Lens
import Servant

import Data.Aeson       ( FromJSON(..) )
import Pixel            ( Error(..), pixelParseJSON )
import Pixel.API.Token  ( Token )
import Pixel.API.Images ( ImageError(..), DigestText, TagList, deleteTags )
import MonadPixel       ( Pixel )

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type DeleteTags =
  Header "Authorization" Token
    :> Capture "uuid" Text
    :> "tags"
    :> ReqBody '[JSON] DeleteTagsRequest
    :> Delete '[JSON] NoContent

--------------------------------------------------------------------------------

newtype DeleteTagsRequest = DeleteTagsRequest
  { deleteTagsRequestTags :: TagList
  } deriving (Show, Generic)

instance FromJSON DeleteTagsRequest where
  parseJSON = pixelParseJSON

makeFields ''DeleteTagsRequest

--------------------------------------------------------------------------------

postDeleteTags
  :: Maybe Token
  -> DigestText
  -> DeleteTagsRequest
  -> Pixel NoContent

postDeleteTags Nothing _ _ = throwError (ImageError MissingToken)
postDeleteTags (Just _) uuid req = do
  deleteTags uuid (req ^. tags)
  pure NoContent
