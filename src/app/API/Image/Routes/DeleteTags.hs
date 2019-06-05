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
import Pixel.Model.Images        ( ImageError(..), DigestText, deleteTags )
import MonadPixel                ( Pixel )

--------------------------------------------------------------------------------

postDeleteTags
  :: Maybe CookieToken
  -> DigestText
  -> Request
  -> Pixel NoContent

postDeleteTags Nothing _ _ = throwError (ImageError MissingToken)
postDeleteTags (Just _) imageUUID req = do
  deleteTags imageUUID (req ^. tags)
  pure NoContent
