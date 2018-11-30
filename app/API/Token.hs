module API.Token
  ( Token(..)
  ) where

import           Protolude
import           Servant                        ( FromHttpApiData(..) )


-- A `Token` generalizes the concept of a user Authentication token, the
-- underlying type shouldn't matter as long as the decoding into this type
-- uniquely identifies an authorized user.
newtype Token = Token
  { tokenText :: Text
  } deriving (Show, Generic, FromHttpApiData)
