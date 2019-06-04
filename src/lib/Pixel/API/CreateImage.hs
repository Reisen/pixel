module Pixel.API.CreateImage
  ( Route
  , Request(..)
  , Response(..)
  ) where

import Protolude
import Servant
import Servant.Multipart
import Data.Aeson        ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types   ( CookieToken )

--------------------------------------------------------------------------------

type Route =
  Header "Cookie" CookieToken
    :> MultipartForm Tmp Request
    :> Post '[JSON] Response

--------------------------------------------------------------------------------

data Request = Request
  { _path :: !Text
  , _tags :: ![Text]
  } deriving (Show, Generic)

data Response = Response
  { _uuid :: !Text
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance FromMultipart Tmp Request where
  fromMultipart multi =
    let allInputs = inputs multi in
    let tagInputs = flip filter allInputs $ (== "tag") . iName in
    let allValues = iValue <$> tagInputs in
    Request
      <$> map (toS . fdPayload) (lookupFile "image" multi)
      <*> pure allValues

instance ToJSON Request where
instance ToJSON Response where
instance FromJSON Request where
instance FromJSON Response where
