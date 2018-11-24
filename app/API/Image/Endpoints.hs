module API.Image.Endpoints
  ( GetImage
  , GetImageByUUID
  , PostImage
  , getImage
  , getImageByUUID
  , postImage
  )
where

import           Protolude
import           Configuration                  ( MonadImageless, HasConnection, HasStaticLocation(..) )
import           Control.Lens
import           Data.Aeson                     ( FromJSON, ToJSON )
import           Data.Data                      ( Data )
import qualified Data.ByteString               as ByteString
import           Eventless                      ( BackendStore )
import           Servant                        ( NoContent(..)
                                                , (:>)
                                                , Capture
                                                , Get
                                                , JSON
                                                , Post
                                                , ReqBody
                                                )

import           API.Image.Persist              ( persistImage )
import           API.Image.Endpoints.PostImage  ( PostImage, PostImageRequest, path, tags )
import           API.Image.Endpoints.GetImage   ( GetImage, GetImageResponse )
import           API.Image.Endpoints.GetImageByUUID
                                                ( GetImageByUUID
                                                , GetImageByUUIDResponse
                                                )


postImage
  :: MonadImageless m
  => MonadReader config m
  => HasConnection config BackendStore
  => HasStaticLocation config Text
  => PostImageRequest
  -> m NoContent

postImage image = do
  config    <- ask
  content   <- liftIO $ ByteString.readFile (image ^. path)
  let static = toS $ config ^. staticLocation <> "doodar"
  liftIO $ ByteString.writeFile static content
  putText "Welcome to the NHK"
  putText $ show $ image ^. tags
  putText $ show $ image ^. path
  pure NoContent


getImage
  :: MonadImageless m
  => m [GetImageResponse]

getImage = do
  putText "Mother Fucker"
  undefined


getImageByUUID
  :: MonadImageless m
  => Text
  -> m GetImageByUUIDResponse

getImageByUUID _ = do
  putText "Mother Fucker"
  undefined
