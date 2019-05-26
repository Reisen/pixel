module Pixel.API.Users.Types.Role
  ( Role (..)
  , RoleEvent (..)
  , Permission (..)
  , makePermission
  ) where

import Protolude
import Data.Aeson           ( ToJSON (..), FromJSON (..) )
import Data.Attoparsec.Text ( parse, maybeResult, many1, anyChar, notChar )
import Data.Data            ( Data )
import Data.List            ( nub, delete )
import Data.Time            ( UTCTime )
import Eventless            ( Events, Project (..) )
import Pixel.JSON           ( pixelToJSON, pixelToEncoding, pixelParseJSON )

--------------------------------------------------------------------------------

data Permission = Permission
  { _permissionTarget :: !Text
  , _permissionScope  :: !Text
  } deriving (Eq, Show, Generic, Typeable, Data)

makePermission :: Text -> Maybe Permission
makePermission encoded = maybeResult . flip parse encoded $ do
  target <- many1 (notChar ':')
  _      <- anyChar
  scope  <- many1 anyChar
  pure $ Permission
    { _permissionTarget = toS $ target
    , _permissionScope  = toS $ scope
    }

data Role = Role
  { _roleName        :: !Text
  , _rolePermissions :: ![Permission]
  , _roleDeletedAt   :: !(Maybe UTCTime)
  } deriving (Eq, Show, Generic, Typeable, Data)

--------------------------------------------------------------------------------

instance ToJSON Role where
  toJSON     = pixelToJSON
  toEncoding = pixelToEncoding

instance ToJSON Permission where
  toJSON     = pixelToJSON
  toEncoding = pixelToEncoding

instance FromJSON Role where
  parseJSON  = pixelParseJSON

instance FromJSON Permission where
  parseJSON  = pixelParseJSON

--------------------------------------------------------------------------------

data RoleEvent
  = RolePermAdded !Permission
  | RolePermRemoved !Permission
  | RoleDeletedAt !UTCTime
  deriving (Show, Generic, Typeable, Data)

type instance Events Role = RoleEvent

instance Project Role where
  foldEvent role = \case
    RolePermAdded v   -> role { _rolePermissions = nub (_rolePermissions role <> [v]) }
    RolePermRemoved v -> role { _rolePermissions = delete v (_rolePermissions role) }
    RoleDeletedAt v   -> role { _roleDeletedAt   = Just v }
