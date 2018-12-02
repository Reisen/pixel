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
import           Data.UUID                      ( fromText )
import           Eventless                      ( BackendStore )
import           Servant                        ( NoContent(..) )

import           API.Token                      ( Token(..) )
import           API.Image.Types                ( Image(..) )
import           API.Image.Persist              ( persistImage )
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
  digest <- storeImageOnDisk image
  persistImageMetadata token image digest
  pure NoContent

  where
    storeImageOnDisk image = do
      static  <- view staticLocation
      content <- liftIO $ ByteString.readFile (image ^. path)
      let digest = show (hash content :: Digest SHA1)
      liftIO $ ByteString.writeFile (toS static <> "/" <> toS digest) content
      pure digest

    persistImageMetadata mayToken image hash =
      case fromText . tokenText $ mayToken of
        Nothing    -> pure ()
        Just token -> persistImage Image
          { _imageHash     = hash
          , _imageTags     = image ^. tags
          , _imageUploader = Just token
          }


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
