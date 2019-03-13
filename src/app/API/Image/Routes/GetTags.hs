module API.Image.Routes.GetTags
  ( GetTags
  , getTags
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Servant

import qualified Data.Aeson                    as A
import qualified Pixel                         as Pixel
import qualified MonadPixel                    as C

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type GetTags =
  Header "Authorization" Pixel.Token
    :> Capture "uuid" Text
    :> "tags"
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

newtype Response = Response
  { responseTags :: Pixel.TagList
  } deriving (Show, Generic)

instance A.ToJSON Response where
  toEncoding = Pixel.pixelToEncoding
  toJSON     = Pixel.pixelToJSON

--------------------------------------------------------------------------------

getTags
  :: Maybe Pixel.Token
  -> Pixel.DigestText
  -> C.Pixel Response

getTags Nothing _     = throwError (Pixel.ImageError Pixel.MissingToken)
getTags (Just _) uuid =
  Pixel.fetchTags uuid >>= \case
    Nothing       -> throwError (Pixel.ImageError Pixel.InvalidUUID)
    Just response -> pure (Response response)
