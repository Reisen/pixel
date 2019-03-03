module API.Image.Routes.PostImage
  ( PostImage
  , postImage
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens
import           Servant
import           Servant.Multipart

import qualified API.Image.Error               as API
import qualified API.Image.Types               as API
import qualified API.Image.Services            as API
import qualified API.Token                     as API
import qualified Configuration                 as C
import qualified Crypto.Hash                   as H
import qualified Data.Aeson                    as A
import qualified Data.ByteString               as B
import qualified Data.UUID                     as U
import qualified Data.UUID.V4                  as U
import qualified Data.Time                     as T
import qualified Error                         as E
import qualified JSON                          as J

--------------------------------------------------------------------------------

-- We receive incoming images as a multi-part form so we can receive file data
-- from the sender.
type PostImage =
  Header "Authorization" API.Token
    :> MultipartForm Tmp Request
    :> Post '[JSON] Text

--------------------------------------------------------------------------------

-- Define a type to wrap up the MultipartData coming over the wire.
data Request = Request
  { requestPath :: !FilePath
  , requestTags :: ![Text]
  } deriving (Show, Generic)

instance A.ToJSON Request where
  toEncoding = J.pixelToEncoding
  toJSON     = J.pixelToJSON

makeFields ''Request

--------------------------------------------------------------------------------

-- Provide an implementation to parse incoming data into our type above.
instance FromMultipart Tmp Request where
  fromMultipart multi =
    let allInputs = inputs multi in
    let tagInputs = flip filter allInputs $ (== "tag") . iName in
    let allValues = iValue <$> tagInputs in
    Request
      <$> map fdPayload (lookupFile "image" multi)
      <*> pure allValues

--------------------------------------------------------------------------------

-- We use SHA3 (224) as our deduplication hash function. Changing this can
-- cause duplications (potentially wanted?)
digestImage :: ByteString -> Text
digestImage = show . (H.hash :: ByteString -> H.Digest H.SHA3_224)

-- Given all uploaded data, produce a valid API image type.
processImage :: T.UTCTime -> [Text] -> API.Token -> ByteString -> API.Image
processImage createdAt newTags token content = API.Image
  { _imageHash      = digestImage content
  , _imageTags      = newTags
  , _imageUploader  = U.fromText . API._tokenText $ token
  , _imageCreatedAt = Just createdAt
  }

--------------------------------------------------------------------------------

-- Endpoint responsible for handling image uploads, it receives an image as a
-- request object, copies the uploaded content to our local static directory
-- and persists meta information to the DB.
postImage :: Maybe API.Token -> Request -> C.Pixel Text
postImage Nothing _        = throwError (E.ImageError API.MissingToken)
postImage (Just token) req = do
  directory <- view C.configStaticLocation
  content   <- liftIO . B.readFile $ req ^. path
  uuid      <- liftIO U.nextRandom
  createdAt <- liftIO T.getCurrentTime
  handleImageUpload $ ImageDetails content directory (req ^. tags) token uuid createdAt
  pure (show uuid)

--------------------------------------------------------------------------------

-- A structure to wrap up our handler function's arguments to keep the type
-- signature a bite cleaner.
data ImageDetails = ImageDetails
  { content      :: ByteString
  , createdAt    :: T.UTCTime
  , directory    :: API.BaseDirectory
  , token        :: API.Token
  , uploadedTags :: API.TagList
  , uuid         :: U.UUID
  }

-- Actual logic for handling an upload.
handleImageUpload
  :: Monad m
  => API.MonadStatic m
  => API.MonadImage m
  => ImageDetails
  -> m ()

handleImageUpload ImageDetails{..} = do
  let digest = digestImage content
  let image  = processImage createdAt uploadedTags token content
  API.writeStaticImage directory digest content
  API.saveImage uuid image
