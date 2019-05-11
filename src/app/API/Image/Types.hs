module API.Image.Types
  ( Image (..)
  ) where

--------------------------------------------------------------------------------

import Protolude

import Data.Aeson       ( ToJSON(..) )
import Pixel            ( pixelToEncoding, pixelToJSON )
import Pixel.API.Images ( DigestText )

--------------------------------------------------------------------------------

data Image = Image
  { imagePath  :: !Text
  , imageThumb :: !Text
  , imageTags  :: ![Text]
  , imageUUID  :: !DigestText
  } deriving (Show, Generic)

instance ToJSON Image where
  toEncoding = pixelToEncoding
  toJSON     = pixelToJSON
