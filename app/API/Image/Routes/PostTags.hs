module API.Image.Routes.PostTags
  ( PostTags
  , postTags
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens
import           Servant

import qualified API.Image.Error               as API
import qualified Data.Aeson                    as A
import qualified Data.UUID                     as U
import qualified Error                         as E
import qualified JSON                          as J
import qualified Pixel                         as Pixel
import qualified MonadPixel                    as C

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type PostTags =
  Header "Authorization" Pixel.Token
    :> Capture "uuid" Text
    :> "tags"
    :> ReqBody '[JSON] Request
    :> Post '[JSON] NoContent

--------------------------------------------------------------------------------

newtype Request = Request
  { requestTags :: Pixel.TagList
  } deriving (Show, Generic)

instance A.FromJSON Request where
  parseJSON = J.pixelParseJSON

makeFields ''Request

--------------------------------------------------------------------------------

postTags
  :: Maybe Pixel.Token
  -> Pixel.DigestText
  -> Request
  -> C.Pixel NoContent

postTags Nothing _ _ = throwError (E.ImageError API.MissingToken)
postTags (Just _) uuid req =
  handleTagsRequest uuid (req ^. tags) >> pure NoContent

--------------------------------------------------------------------------------

handleTagsRequest
  :: Monad m
  => Pixel.MonadImage m
  => Pixel.DigestText
  -> Pixel.TagList
  -> m ()

handleTagsRequest uuidText newTags = case U.fromText uuidText of
  Nothing   -> pure ()
  Just uuid -> Pixel.appendTags uuid newTags
