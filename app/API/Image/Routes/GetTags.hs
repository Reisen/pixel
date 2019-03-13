module API.Image.Routes.GetTags
  ( GetTags
  , getTags
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
  toEncoding = J.pixelToEncoding
  toJSON     = J.pixelToJSON

--------------------------------------------------------------------------------

getTags
  :: Maybe Pixel.Token
  -> Pixel.DigestText
  -> C.Pixel Response

getTags Nothing _     = throwError (E.ImageError API.MissingToken)
getTags (Just _) uuid = handleTagsRequest uuid >>= \case
  Nothing       -> throwError (E.ImageError API.InvalidUUID)
  Just response -> pure (Response response)

--------------------------------------------------------------------------------

handleTagsRequest
  :: Monad m
  => Pixel.MonadImage m
  => Pixel.DigestText
  -> m (Maybe Pixel.TagList)

handleTagsRequest uuidText = case U.fromText uuidText of
  Nothing   -> pure Nothing
  Just uuid -> Pixel.loadImage uuid >>= \case
    Nothing    -> pure Nothing
    Just image -> pure . Just $ image ^. Pixel.imageTags
