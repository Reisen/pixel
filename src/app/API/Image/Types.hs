module API.Image.Types
  ( Image (..)
  ) where

--------------------------------------------------------------------------------

import Protolude

import Data.Aeson ( ToJSON (..) )
import Pixel      ( DigestText, pixelToEncoding, pixelToJSON )

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
