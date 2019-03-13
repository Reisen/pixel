module API.Image.Routes.DeleteTags
  ( DeleteTags
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
    :> ReqBody '[JSON] Request
    :> Delete '[JSON] NoContent

--------------------------------------------------------------------------------

newtype Request = Request
  { requestTags :: Pixel.TagList
  } deriving (Show, Generic)

instance A.FromJSON Request where
  parseJSON = Pixel.pixelParseJSON

makeFields ''Request

--------------------------------------------------------------------------------

deleteTags
  :: Maybe Pixel.Token
  -> Pixel.DigestText
  -> Request
  -> C.Pixel NoContent

deleteTags Nothing _ _ = throwError (Pixel.ImageError Pixel.MissingToken)
deleteTags (Just _) uuid req = do
  Pixel.deleteTags uuid (req ^. tags)
  pure NoContent
