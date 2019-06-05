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
import Pixel.Model.Images        ( DigestText, ImageError(..), addTags )

--------------------------------------------------------------------------------

postTags
  :: Maybe CookieToken
  -> DigestText
  -> Request
  -> Pixel NoContent

postTags Nothing _ _            = throwError (ImageError MissingToken)
postTags (Just _) imageUUID req = do
  addTags imageUUID (req ^. tags)
  pure NoContent
