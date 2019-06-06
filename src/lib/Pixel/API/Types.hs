module Pixel.API.Types
  ( CookieToken(..)
  , APIImage(..)
  ) where

import Protolude
import Servant
import Data.Aeson         ( ToJSON(..), FromJSON(..) )
import Data.List          ( lookup )
import Data.Text.Encoding ( encodeUtf8 )
import Pixel.Model.Token  ( Token(..) )
import Web.Cookie         ( parseCookiesText )

--------------------------------------------------------------------------------

-- Servant doesn't really give an easy way to access specific cookies using its
-- combinators. Instead, we wrap the `cookie` page `CookiesText` type and parse
-- it ourself into a [(Text, Text)], then scan it for the cookie we care about.

newtype CookieToken = CookieToken
  { cookieTokenToken :: Token
  } deriving (Show)


instance FromHttpApiData CookieToken where
  parseHeader header =
    let cookies  = parseCookiesText header in
    let mayToken = lookup "token" cookies in
    case mayToken of
      Nothing    -> Left "Token Missing"
      Just token -> Right . CookieToken . Token $ token

  parseQueryParam = parseHeader . encodeUtf8

--------------------------------------------------------------------------------

data APIImage = APIImage
  { _dimensions :: (Int, Int)
  , _filename   :: !Text
  , _filesize   :: !Int
  , _path       :: !Text
  , _tags       :: ![Text]
  , _thumb      :: !Text
  , _uuid       :: !Text
  } deriving (Show, Generic)

instance ToJSON APIImage where
instance FromJSON APIImage where
