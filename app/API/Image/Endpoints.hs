module API.Image.Endpoints
  ( GetImage
  , GetImageByUUID
  , PostImage
  , getImage
  , getImageByUUID
  , postImage
  )
where

import           Protolude               hiding ( hash )
import           Crypto.Hash                    ( Digest, SHA1, hash )
import           Configuration                  ( MonadImageless, HasConnection, HasStaticLocation(..) )
import           Control.Lens
import qualified Data.ByteString               as ByteString
import           Eventless                      ( BackendStore )
import           Servant                        ( NoContent(..) )

import           API.Token                      ( Token )
import           API.Image.Persist              ( PersistImage(..), persistImage )
import           API.Image.Endpoints.PostImage  ( PostImage, PostImageRequest, path, tags )
import           API.Image.Endpoints.GetImage   ( GetImage, GetImageResponse )
import           API.Image.Endpoints.GetImageByUUID ( GetImageByUUID , GetImageByUUIDResponse )

--------------------------------------------------------------------------------

-- Endpoint responsible for handling image uploads, it receives an image as a
-- request object, copies the uploaded content to our local static directory
-- and persists meta information to the DB.
postImage
  :: MonadImageless m
  => MonadReader config m
  => HasConnection config BackendStore
  => HasStaticLocation config Text
  => Maybe Token
  -> PostImageRequest
  -> m NoContent

postImage Nothing _          = pure NoContent
postImage (Just token) image = do
  -- Static Location
  putText "WTF"
  static    <- view staticLocation
  content   <- liftIO $ ByteString.readFile (image ^. path)
  let hashed = show (hash content :: Digest SHA1)
  liftIO $ ByteString.writeFile (toS static <> "/" <> toS hashed) content

  -- Persist Image to Disk
  persistImage PersistImage
    { _piUser = token
    , _piHash = hashed
    , _piTags = image ^. tags
    }

  pure NoContent


-- Fetch images from the backend, this endpoint accepts a filter to decide what
-- to return from the backend. By default this is all images ordered by upload
-- date and the filter should encompass all possible queries a user might have,
-- from tags to uploader, to ordering.
getImage
  :: MonadImageless m
  => Maybe Token
  -> m [GetImageResponse]

getImage _token = do
  putText "Mother Fucker"
  undefined


getImageByUUID
  :: MonadImageless m
  => Maybe Token
  -> Text
  -> m GetImageByUUIDResponse

getImageByUUID _token _ = do
  putText "Mother Fucker"
  undefined
