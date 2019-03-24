module API.Image.Routes.DeleteTags
  ( DeleteTags
  , DeleteTagsRequest
  , deleteTags
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
type DeleteTags =
  Header "Authorization" Pixel.Token
    :> Capture "uuid" Text
    :> "tags"
    :> ReqBody '[JSON] DeleteTagsRequest
    :> Delete '[JSON] NoContent

--------------------------------------------------------------------------------

newtype DeleteTagsRequest = DeleteTagsRequest
  { deleteTagsRequestTags :: Pixel.TagList
  } deriving (Show, Generic)

instance A.FromJSON DeleteTagsRequest where
  parseJSON = Pixel.pixelParseJSON

makeFields ''DeleteTagsRequest

--------------------------------------------------------------------------------

deleteTags
  :: Maybe Pixel.Token
  -> Pixel.DigestText
  -> DeleteTagsRequest
  -> C.Pixel NoContent

deleteTags Nothing _ _ = throwError (Pixel.ImageError Pixel.MissingToken)
deleteTags (Just _) uuid req = do
  Pixel.deleteTags uuid (req ^. tags)
  pure NoContent
