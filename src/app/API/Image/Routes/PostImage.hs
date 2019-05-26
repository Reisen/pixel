module API.Image.Routes.PostImage
  ( PostImage
  , PostImageRequest
  , postImage
  )
where

import Protolude
import Control.Lens
import Servant
import Servant.Multipart

import Configuration        ( configStaticLocation )
import MonadPixel           ( Pixel )
import Data.Aeson           ( ToJSON(..) )
import Data.ByteString as B ( readFile )
import Data.UUID.V4         ( nextRandom )
import Data.Time            ( getCurrentTime )
import Pixel                ( Error(..), pixelToEncoding, pixelToJSON )
import Pixel.API.Token      ( Token )
import Pixel.API.Images     ( ImageError(..), ImageDetails(..), handleImageUpload )

--------------------------------------------------------------------------------

-- We receive incoming images as a multi-part form so we can receive file data
-- from the sender.
type PostImage =
  Header "Authorization" Token
    :> MultipartForm Tmp PostImageRequest
    :> Post '[JSON] Text

--------------------------------------------------------------------------------

-- Define a type to wrap up the MultipartData coming over the wire.
data PostImageRequest = PostImageRequest
  { postImageRequestPath :: !FilePath
  , postImageRequestTags :: ![Text]
  } deriving (Show, Generic)

instance ToJSON PostImageRequest where
  toEncoding = pixelToEncoding
  toJSON     = pixelToJSON

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
  :: Maybe Token
  -> PostImageRequest
  -> Pixel Text

postImage Nothing _        = throwError (ImageError MissingToken)
postImage (Just token) req = do
  putText (show req)
  directory <- view configStaticLocation
  content   <- liftIO . B.readFile $ req ^. path
  uuid      <- liftIO nextRandom
  createdAt <- liftIO getCurrentTime
  handleImageUpload $ ImageDetails content createdAt directory token (req ^. tags) uuid
  pure (show uuid)
