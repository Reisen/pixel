module API.Image.Routes.PostImage
  ( PostImage
  , PostImageRequest
  , postImage
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens
import           Servant
import           Servant.Multipart

import qualified Configuration                 as C
import qualified Data.Aeson                    as A
import qualified Data.ByteString               as B
import qualified Data.UUID.V4                  as U
import qualified Data.Time                     as T
import qualified Pixel                         as Pixel
import qualified MonadPixel                    as C

--------------------------------------------------------------------------------

-- We receive incoming images as a multi-part form so we can receive file data
-- from the sender.
type PostImage =
  Header "Authorization" Pixel.Token
    :> MultipartForm Tmp PostImageRequest
    :> Post '[JSON] Text

--------------------------------------------------------------------------------

-- Define a type to wrap up the MultipartData coming over the wire.
data PostImageRequest = PostImageRequest
  { postImageRequestPath :: !FilePath
  , postImageRequestTags :: ![Text]
  } deriving (Show, Generic)

instance A.ToJSON PostImageRequest where
  toEncoding = Pixel.pixelToEncoding
  toJSON     = Pixel.pixelToJSON

makeFields ''PostImageRequest

--------------------------------------------------------------------------------

-- Provide an implementation to parse incoming data into our type above.
instance FromMultipart Tmp PostImageRequest where
  fromMultipart multi =
    let allInputs = inputs multi in
    let tagInputs = flip filter allInputs $ (== "tag") . iName in
    let allValues = iValue <$> tagInputs in
    PostImageRequest
      <$> map fdPayload (lookupFile "image" multi)
      <*> pure allValues

--------------------------------------------------------------------------------

-- Endpoint responsible for handling image uploads, it receives an image as a
-- request object, copies the uploaded content to our local static directory
-- and persists meta information to the DB.
postImage
  :: Maybe Pixel.Token
  -> PostImageRequest
  -> C.Pixel Text

postImage Nothing _        = throwError (Pixel.ImageError Pixel.MissingToken)
postImage (Just token) req = do
  directory <- view C.configStaticLocation
  content   <- liftIO . B.readFile $ req ^. path
  uuid      <- liftIO U.nextRandom
  createdAt <- liftIO T.getCurrentTime
  Pixel.handleImageUpload $ Pixel.ImageDetails content createdAt directory token (req ^. tags) uuid
  pure (show uuid)
