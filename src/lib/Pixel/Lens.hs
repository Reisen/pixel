module Pixel.Lens where

import Control.Lens
import Pixel.Model.Images.Types
import Pixel.Model.Users.Types.Email
import Pixel.Model.Users.Types.Password
import Pixel.Model.Users.Types.User
import Pixel.Model.Users.Types.Role

import Pixel.API.AppendImageTags         as AppendImageTags

--------------------------------------------------------------------------------

-- Create HasX style lenses for core model types.

makeFieldsNoPrefix ''Email
makeFieldsNoPrefix ''Image
makeFieldsNoPrefix ''Password
makeFieldsNoPrefix ''Permission
makeFieldsNoPrefix ''Role
makeFieldsNoPrefix ''User
makePrisms         ''UserState
makePrisms         ''VerifyPasswordResult

--------------------------------------------------------------------------------

-- Create HasX style lenses for Request/Response types.

makeFieldsNoPrefix ''AppendImageTags.Request
