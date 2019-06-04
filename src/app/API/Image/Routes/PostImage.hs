module API.Image.Routes.PostImage
  ( postImage
  ) where

import Protolude
import Control.Lens
import Configuration         ( configStaticLocation )
import MonadPixel            ( Pixel )
import Data.ByteString as B  ( readFile )
import Data.UUID.V4          ( nextRandom )
import Data.Time             ( getCurrentTime )
import Pixel                 ( Error(..) )
import Pixel.API             ( CookieToken(..) )
import Pixel.API.CreateImage ( Request(..), Response(..) )
import Pixel.Model.Images    ( ImageError(..), ImageDetails(..), createImage )

--------------------------------------------------------------------------------

-- | Endpoint responsible for handling image uploads, it receives an image as a
-- | request object, copies the uploaded content to our local static directory
-- | and persists meta information to the DB.

postImage
  :: Maybe CookieToken
  -> Request
  -> Pixel Response

postImage Nothing _                              = throwError (ImageError MissingToken)
postImage (Just (CookieToken token)) Request{..} = do
  directory <- view configStaticLocation
  content   <- liftIO . B.readFile . toS $ _path
  uuid      <- liftIO nextRandom
  createdAt <- liftIO getCurrentTime
  createImage $ ImageDetails content createdAt directory token _tags uuid
  pure . Response . show $ uuid
