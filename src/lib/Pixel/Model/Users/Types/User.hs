module Pixel.Model.Users.Types.User
  ( User (..)
  , UserState (..)
  , UserEvent (..)
  ) where

import Protolude
import Data.Aeson                       ( ToJSON (..), FromJSON (..) )
import Data.Data                        ( Data )
import Data.Default.Class               ( Default, def )
import Data.Time                        ( UTCTime )
import Data.UUID                        ( UUID )
import Eventless                        ( Events, Project (..) )
import Pixel.Model.Users.Types.Email    ( Email )
import Pixel.Model.Users.Types.Password ( Password )
import Pixel.JSON                       ( pixelToJSON, pixelToEncoding, pixelParseJSON )

--------------------------------------------------------------------------------

data UserState
  = Unverified
  | Verified
  | Banned
  deriving (Show, Generic, Typeable, Data)

data User = User
  { _username     :: !(Maybe Text)
  , _password     :: !(Maybe Password)
  , _avatar       :: !Text
  , _role         :: !(Maybe UUID)
  , _state        :: !UserState
  , _email        :: !(Maybe Email)
  , _registeredAt :: !(Maybe UTCTime)
  , _deletedAt    :: !(Maybe UTCTime)
  } deriving (Show, Generic, Typeable, Data)

--------------------------------------------------------------------------------

data UserEvent
  = CreatedAt !UTCTime
  | DeletedAt !UTCTime
  | UsernameChanged !Text
  | PasswordChanged !Password
  | EmailChanged !Email
  | StateChanged !UserState
  | RoleChanged !UUID
  deriving (Show, Generic, Typeable, Data)

type instance Events User = UserEvent

--------------------------------------------------------------------------------

instance Default UserState where
  def = Unverified

instance Default User where
  def = User
    { _username     = Nothing
    , _password     = Nothing
    , _avatar       = mempty
    , _role         = Nothing
    , _state        = Unverified
    , _email        = mempty
    , _registeredAt = Nothing
    , _deletedAt    = Nothing
    }

--------------------------------------------------------------------------------

instance ToJSON User where
  toJSON     = pixelToJSON
  toEncoding = pixelToEncoding

instance ToJSON UserState where
  toJSON     = pixelToJSON
  toEncoding = pixelToEncoding

instance FromJSON User where
  parseJSON  = pixelParseJSON

instance FromJSON UserState where
  parseJSON  = pixelParseJSON

instance ToJSON UserEvent where
instance FromJSON UserEvent where

--------------------------------------------------------------------------------

instance Project User where
  foldEvent user = \case
    CreatedAt v       -> user { _registeredAt = Just v }
    DeletedAt v       -> user { _deletedAt    = Just v }
    EmailChanged v    -> user { _email        = Just v }
    UsernameChanged v -> user { _username     = Just v }
    PasswordChanged v -> user { _password     = Just v }
    StateChanged v    -> user { _state        = v }
    RoleChanged v     -> user { _role         = Just v }
