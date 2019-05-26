module Pixel.API.Users.Types.Email
  ( Email (..)
  ) where

import Protolude
import Data.Aeson      ( ToJSON (..), FromJSON (..) )
import Data.Data       ( Data )

--------------------------------------------------------------------------------

-- Email's should never be just text, we wrap them here to enforce some kind
-- of validation through construction.
newtype Email = Email
  { emailText :: Text
  } deriving (Show, Generic, Typeable, Data, Semigroup)

--------------------------------------------------------------------------------

instance ToJSON Email where
instance FromJSON Email where
