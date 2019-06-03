module Pixel.Model.Users.Types.Role
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
  { _target :: !Text
  , _scope  :: !Text
  } deriving (Eq, Show, Generic, Typeable, Data)

makePermission :: Text -> Maybe Permission
makePermission encoded = maybeResult . flip parse encoded $ do
  target <- many1 (notChar ':')
  _      <- anyChar
  scope  <- many1 anyChar
  pure $ Permission
    { _target = toS $ target
    , _scope  = toS $ scope
    }

data Role = Role
  { _name        :: !Text
  , _permissions :: ![Permission]
  , _deletedAt   :: !(Maybe UTCTime)
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
    RolePermAdded v   -> role { _permissions = nub (_permissions role <> [v]) }
    RolePermRemoved v -> role { _permissions = delete v (_permissions role) }
    RoleDeletedAt v   -> role { _deletedAt   = Just v }
