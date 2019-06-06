module API.User.Types
  ( User(..)
  , DangerousUser(..)
  ) where

import Protolude
import Data.Aeson ( ToJSON(..), FromJSON(..) )

--------------------------------------------------------------------------------

-- In order to prevent accidental leakage of private data through the API, these
-- types have been named intentionally verbosely to force opt-in when it's known
-- that these values are expected over the wire.

data DangerousUser = DangerousUser
  { _email    :: !Text
  , _password :: !Text
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

-- Steralized User type that is OK for an authing user to receive.
data User = User
  { _uuid     :: !(Maybe Text)
  , _username :: !(Maybe Text)
  , _email    :: !(Maybe Text)
  , _role     :: !(Maybe Text)
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance ToJSON User where
instance FromJSON User where
instance FromJSON DangerousUser where
