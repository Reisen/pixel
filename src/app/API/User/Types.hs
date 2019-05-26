module API.User.Types
  ( User(..)
  , DangerousUser(..)
  ) where

import Protolude

import Data.Aeson      ( ToJSON(..), FromJSON(..) )
import Pixel           ( pixelParseJSON, pixelToEncoding, pixelToJSON )

--------------------------------------------------------------------------------

-- In order to prevent accidental leakage of private data through the API, these
-- types have been named intentionally verbosely to force opt-in when it's known
-- that these values are expected over the wire.
data DangerousUser = DangerousUser
  { dangerousUserEmail    :: !Text
  , dangerousUserPassword :: !Text
  } deriving (Show, Generic)

-- We only implement FromJSON, as another safeguard against accidentally sending
-- user's private data over the wire.
instance FromJSON DangerousUser where
  parseJSON  = pixelParseJSON

-- Steralized User type that is OK for an authing user to receive.
data User = User
  { userUsername :: !(Maybe Text)
  , userEmail    :: !(Maybe Text)
  , userRole     :: !(Maybe Text)
  } deriving (Show, Generic)

instance ToJSON User where
  toEncoding = pixelToEncoding
  toJSON     = pixelToJSON

instance FromJSON User where
  parseJSON  = pixelParseJSON
