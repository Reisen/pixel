module Pixel.Lens where

import Control.Lens
import Pixel.Model.Images.Types
import Pixel.Model.Users.Types.Email
import Pixel.Model.Users.Types.Password
import Pixel.Model.Users.Types.User
import Pixel.Model.Users.Types.Role

--------------------------------------------------------------------------------

makeFieldsNoPrefix ''Image
makeFieldsNoPrefix ''Email
makeFieldsNoPrefix ''Password
makePrisms         ''VerifyPasswordResult
makeFieldsNoPrefix ''Permission
makeFieldsNoPrefix ''Role
makeFieldsNoPrefix ''User
makePrisms         ''UserState
