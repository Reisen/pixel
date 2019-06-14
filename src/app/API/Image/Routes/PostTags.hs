module API.Image.Routes.PostTags
  ( postTags
  ) where

import Protolude
import Pixel.Lens
import Control.Lens
import Servant
import MonadPixel                ( Pixel )
import Pixel                     ( Error(..), CookieToken(..) )
import Pixel.API.AppendImageTags ( Request(..) )
import Pixel.Operations          ( appendImageTags )

--------------------------------------------------------------------------------

postTags
  :: Maybe CookieToken
  -> Text
  -> Request
  -> Pixel NoContent

postTags Nothing _ _            = throwError UnknownError
postTags (Just _) imageUUID req = do
  appendImageTags imageUUID (req ^. tags)
  pure NoContent
