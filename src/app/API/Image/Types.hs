module API.Image.Types
  ( Image (..)
  ) where

--------------------------------------------------------------------------------

import           Protolude
import qualified Pixel

import qualified Data.Aeson                    as A

--------------------------------------------------------------------------------

data Image = Image
  { imagePath  :: !Text
  , imageThumb :: !Text
  , imageTags  :: ![Text]
  , imageUUID  :: !Pixel.DigestText
  } deriving (Show, Generic)

instance A.ToJSON Image where
  toEncoding = Pixel.pixelToEncoding
  toJSON     = Pixel.pixelToJSON
