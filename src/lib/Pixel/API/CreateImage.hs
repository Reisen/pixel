module Pixel.API.CreateImage
  ( CreateImageRoute
  , CreateImageRequest(..)
  , CreateImageResponse(..)
  ) where

import Protolude
import Servant
import Servant.Multipart
import Data.Aeson        ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types   ( CookieToken )

--------------------------------------------------------------------------------

type CreateImageRoute =
  Header "Cookie" CookieToken
    :> MultipartForm Tmp CreateImageRequest
    :> Post '[JSON] CreateImageResponse

--------------------------------------------------------------------------------

data CreateImageRequest = CreateImageRequest
  { _path :: !Text
  , _tags :: ![Text]
  } deriving (Show, Generic)

data CreateImageResponse = CreateImageResponse
  { _uuid :: !Text
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance FromMultipart Tmp CreateImageRequest where
  fromMultipart multi =
    let allInputs = inputs multi in
    let tagInputs = flip filter allInputs $ (== "tag") . iName in
    let allValues = iValue <$> tagInputs in
    CreateImageRequest
      <$> map (toS . fdPayload) (lookupFile "image" multi)
      <*> pure allValues

instance ToJSON CreateImageRequest where
instance ToJSON CreateImageResponse where
instance FromJSON CreateImageRequest where
instance FromJSON CreateImageResponse where
