module API.Cookies
  ( CookieList(..)
  ) where

import Protolude
import Servant
import Data.Text.Encoding ( encodeUtf8 )
import Web.Cookie         ( CookiesText, parseCookiesText )

--------------------------------------------------------------------------------

-- Servant doesn't really give an easy way to access specific cookies using its
-- combinators. Instead, we wrap the `cookie` page `CookiesText` type and parse
-- it ourself into a [(Text, Text)].
newtype CookieList = CookieList
  { cookieListCookies :: CookiesText
  } deriving (Show)


instance FromHttpApiData CookieList where
  parseHeader     = pure . CookieList . parseCookiesText
  parseQueryParam = pure . CookieList . parseCookiesText . encodeUtf8
