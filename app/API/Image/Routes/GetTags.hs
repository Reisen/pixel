module API.Image.Routes.GetTags
  ( GetTags
  , getTags
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens
import           Servant

import qualified API.Image.Types               as API
import qualified API.Image.Services            as API
import qualified API.Token                     as API
import qualified Configuration                 as C
import qualified Data.Aeson                    as A
import qualified Data.UUID                     as U

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type GetTags =
  Header "Authorization" API.Token
    :> Capture "uuid" Text
    :> "tags"
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

newtype Response = Response
  { responseTags :: API.TagList
  } deriving (Show, Generic)

instance A.ToJSON Response where

--------------------------------------------------------------------------------

getTags :: Maybe API.Token -> API.DigestText -> C.Pixel Response
getTags Nothing _     = throwError ()
getTags (Just _) uuid = handleTagsRequest uuid >>= \case
  Nothing       -> throwError ()
  Just response -> pure (Response response)

--------------------------------------------------------------------------------

handleTagsRequest
  :: Monad m
  => API.MonadImage m
  => API.DigestText
  -> m (Maybe API.TagList)

handleTagsRequest uuidText = case U.fromText uuidText of
  Nothing   -> pure Nothing
  Just uuid -> API.loadImage uuid >>= \case
    Nothing    -> pure Nothing
    Just image -> pure . Just $ image ^. API.imageTags
