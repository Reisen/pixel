module Pixel.Model.Token
  ( Token(..)
  , tokenText
  ) where

import Protolude
import Control.Lens
import Data.Aeson   ( ToJSON(..), FromJSON(..) )
import Pixel.JSON   ( pixelToJSON, pixelToEncoding, pixelParseJSON )
import Servant      ( FromHttpApiData(..) )

--------------------------------------------------------------------------------

-- A `Token` generalizes the concept of a user Authentication token, the
-- underlying type shouldn't matter as long as the decoding into this type
-- uniquely identifies an authorized user.
newtype Token = Token
  { _tokenText :: Text
  } deriving (Show, Generic, FromHttpApiData)

--------------------------------------------------------------------------------

instance ToJSON Token where
  toJSON     = pixelToJSON
  toEncoding = pixelToEncoding

instance FromJSON Token where
  parseJSON  = pixelParseJSON

--------------------------------------------------------------------------------

makeLenses ''Token
