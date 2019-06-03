module Pixel.Model.Users.Types.Password
  ( Password(..)
  , VerifyPasswordResult(..)
  , makePassword
  , verifyPassword
  ) where

import Protolude
import Crypto.Argon2   ( Argon2Status(..), defaultHashOptions, hashEncoded, verifyEncoded )
import Data.Aeson      ( ToJSON (..), FromJSON (..) )
import Data.Data       ( Data )
import Data.Text.Short ( fromText, toText )
import Pixel.JSON      ( pixelToJSON, pixelToEncoding, pixelParseJSON )

--------------------------------------------------------------------------------

-- Password's should never be just text, we wrap them here to enforce some kind
-- of validation through construction.
newtype Password = UnsafeMkPassword_
  { _passwordText :: Text
  } deriving (Show, Generic, Typeable, Data)

-- Used to distinguish between a legitimate failure, and a need to rehash from
-- an outdated hash.
data VerifyPasswordResult
  = VerifySucceeded
  | VerifyFailed
  | VerifyRehashRequired
  | VerifyUnknownFailure !Text
  deriving (Show, Generic, Typeable, Data)

--------------------------------------------------------------------------------

instance ToJSON Password where

instance ToJSON VerifyPasswordResult where
  toJSON     = pixelToJSON
  toEncoding = pixelToEncoding

instance FromJSON Password where

instance FromJSON VerifyPasswordResult where
  parseJSON  = pixelParseJSON

--------------------------------------------------------------------------------

-- Creation of a Password is abstracted away, so that it can be changed over
-- time without code changes.
makePassword
  :: Text
  -> Text
  -> Maybe Password

makePassword password salt =
  case hashEncoded defaultHashOptions (toS password) (toS salt) of
    Left _       -> Nothing
    Right result -> Just . UnsafeMkPassword_ $ toText result


-- Verification of a password is also abstracted, so that we can verify
-- multiple possibilities in one place. If we change from Argon2 to Lyra for
-- example, we can update this function to first test Lyra, and fallback to
-- Argon2 without any code relying on it changing.
verifyPassword
  :: Password
  -> Text
  -> VerifyPasswordResult

verifyPassword (UnsafeMkPassword_ password) candidate =
  case verifyEncoded (fromText password) (toS candidate) of
    Argon2Ok             -> VerifySucceeded
    Argon2VerifyMismatch -> VerifyFailed
    err                  -> VerifyUnknownFailure (show err)
