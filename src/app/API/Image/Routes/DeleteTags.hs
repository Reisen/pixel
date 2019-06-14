module API.Image.Routes.DeleteTags
  ( postDeleteTags
  ) where

import Protolude
import Control.Lens
import Servant
import Pixel                     ( Error(..) )
import Pixel.API                 ( CookieToken(..) )
import Pixel.API.DeleteImageTags ( Request(..) )
import Pixel.Lens
import Pixel.Operations          ( removeImageTags )
import MonadPixel                ( Pixel )

--------------------------------------------------------------------------------

postDeleteTags
  :: Maybe CookieToken
  -> Text
  -> Request
  -> Pixel NoContent

postDeleteTags Nothing _ _            = throwError UnknownError
postDeleteTags (Just _) imageUUID req = do
  removeImageTags imageUUID (req ^. tags)
  pure NoContent
