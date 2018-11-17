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
import           Configuration                  ( MonadImageless, HasConnection )
import           Control.Lens
import           Data.Aeson                     ( FromJSON, ToJSON )
import           Data.Data                      ( Data )
import           Data.UUID.V4                   ( nextRandom )
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
  => PostImageRequest
  -> m NoContent

postImage image = do
  uuid    <- liftIO nextRandom
  content <- liftIO $ readFile (image ^. path)
  putText "Welcome to the NHK"
  putText content
  putText $ show $ image ^. tags
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
