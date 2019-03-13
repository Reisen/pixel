module API.Image.Routes.PostTags
  ( PostTags
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
    :> ReqBody '[JSON] Request
    :> Post '[JSON] NoContent

--------------------------------------------------------------------------------

newtype Request = Request
  { requestTags :: Pixel.TagList
  } deriving (Show, Generic)

instance A.FromJSON Request where
  parseJSON = Pixel.pixelParseJSON

makeFields ''Request

--------------------------------------------------------------------------------

postTags
  :: Maybe Pixel.Token
  -> Pixel.DigestText
  -> Request
  -> C.Pixel NoContent

postTags Nothing _ _ = throwError (Pixel.ImageError Pixel.MissingToken)
postTags (Just _) uuid req = do
  Pixel.addTags uuid (req ^. tags)
  pure NoContent
