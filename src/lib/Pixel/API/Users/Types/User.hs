module Pixel.API.Users.Types.User
  ( User (..)
  , UserState (..)
  , UserEvent (..)

  -- Lenses
  , userUsername
  , userPassword
  , userAvatar
  , userRole
  , userState
  , userEmail
  , userRegisteredAt
  , userDeletedAt

  -- Prisms
  , _Unverified
  , _Verified
  , _Banned
  , _CreatedAt
  , _DeletedAt
  , _UsernameChanged
  , _PasswordChanged
  , _EmailChanged
  , _StateChanged
  , _RoleChanged
  ) where

import Protolude
import Control.Lens
import Data.Aeson                     ( ToJSON (..), FromJSON (..) )
import Data.Data                      ( Data )
import Data.Default.Class             ( Default, def )
import Data.Time                      ( UTCTime )
import Data.UUID                      ( UUID )
import Eventless                      ( Events, Project (..) )
import Pixel.API.Users.Types.Email    ( Email )
import Pixel.API.Users.Types.Password ( Password )
import Pixel.JSON                     ( pixelToJSON, pixelToEncoding, pixelParseJSON )

--------------------------------------------------------------------------------

data UserState
  = Unverified
  | Verified
  | Banned
  deriving (Show, Generic, Typeable, Data)

data User = User
  { _userUsername     :: !(Maybe Text)
  , _userPassword     :: !(Maybe Password)
  , _userAvatar       :: !Text
  , _userRole         :: !(Maybe UUID)
  , _userState        :: !UserState
  , _userEmail        :: !(Maybe Email)
  , _userRegisteredAt :: !(Maybe UTCTime)
  , _userDeletedAt    :: !(Maybe UTCTime)
  } deriving (Show, Generic, Typeable, Data)

makeLenses ''User
makePrisms ''UserState

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

makePrisms ''UserEvent

--------------------------------------------------------------------------------

instance Default UserState where
  def = Unverified

instance Default User where
  def = User
    { _userUsername     = Nothing
    , _userPassword     = Nothing
    , _userAvatar       = mempty
    , _userRole         = Nothing
    , _userState        = Unverified
    , _userEmail        = mempty
    , _userRegisteredAt = Nothing
    , _userDeletedAt    = Nothing
    }

--------------------------------------------------------------------------------

instance ToJSON User where
  toJSON     = pixelToJSON
  toEncoding = pixelToEncoding

instance ToJSON UserState where
  toJSON     = pixelToJSON
  toEncoding = pixelToEncoding

instance ToJSON UserEvent where

instance FromJSON User where
  parseJSON  = pixelParseJSON

instance FromJSON UserState where
  parseJSON  = pixelParseJSON

instance FromJSON UserEvent where

--------------------------------------------------------------------------------

instance Project User where
  foldEvent user = \case
    CreatedAt v       -> user { _userRegisteredAt = Just v }
    DeletedAt v       -> user { _userDeletedAt    = Just v }
    EmailChanged v    -> user { _userEmail        = Just v }
    UsernameChanged v -> user { _userUsername     = Just v }
    PasswordChanged v -> user { _userPassword     = Just v }
    StateChanged v    -> user { _userState        = v }
    RoleChanged v     -> user { _userRole         = Just v }
