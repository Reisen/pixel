module Pixel.Lens where

import Control.Lens
import Pixel.API.Types
import Pixel.Model.Images.Types
import Pixel.Model.Users.Types.Email
import Pixel.Model.Users.Types.Password
import Pixel.Model.Users.Types.User
import Pixel.Model.Users.Types.Role

import qualified Pixel.API.AppendImageTags  as AppendImageTags
import qualified Pixel.API.CreateImage      as CreateImage
import qualified Pixel.API.DeleteImageTags  as DeleteImageTags
import qualified Pixel.API.FetchImages      as FetchImages
import qualified Pixel.API.FetchImageByUUID as FetchImageByUUID
import qualified Pixel.API.FetchImageTags   as FetchImageTags

--------------------------------------------------------------------------------

-- Create HasX style lenses for core Model Types.

makeFieldsNoPrefix ''Email
makeFieldsNoPrefix ''Image
makeFieldsNoPrefix ''Password
makeFieldsNoPrefix ''Permission
makeFieldsNoPrefix ''Role
makeFieldsNoPrefix ''User
makePrisms         ''UserState
makePrisms         ''VerifyPasswordResult

--------------------------------------------------------------------------------

-- Create HasX style lenses for API Types.

makeFieldsNoPrefix ''Pixel.API.Types.APIImage

--------------------------------------------------------------------------------

-- Create HasX style lenses for Request/Response types.

makeFieldsNoPrefix ''AppendImageTags.Request
makeFieldsNoPrefix ''CreateImage.Request
makeFieldsNoPrefix ''CreateImage.Response
makeFieldsNoPrefix ''DeleteImageTags.Request
makeFieldsNoPrefix ''FetchImageByUUID.Response
makeFieldsNoPrefix ''FetchImageTags.Response
makeFieldsNoPrefix ''FetchImages.Response
