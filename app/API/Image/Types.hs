module API.Image.Types
  ( Image
  , ImageEvent (..)
  , HasHash    (..)
  , HasPath    (..)
  , HasTags    (..)
  ) where

import Control.Lens
import Data.Aeson         (FromJSON, ToJSON)
import Data.Data          (Data)
import Data.Default.Class (Default, def)
import Data.List          (nub, delete)
import Eventless          (Project (..), Events)
import Protolude hiding
  ( hash
  )


-- Define Core Data
data Image = Image
  { _imageHash :: Text
  , _imagePath :: Text
  , _imageTags :: [Text]
  }
  deriving (Show, Generic, Typeable, Data)


-- Derive Lens
makeFields ''Image


-- Derive Event Sourcing Backend
data ImageEvent
  = HashChanged Text
  | PathChanged Text
  | TagAppended Text
  | TagRemoved  Text
  deriving (Show, Generic, Typeable, Data)

type instance Events Image = ImageEvent


-- Derive JSON
instance ToJSON Image
instance ToJSON ImageEvent

instance FromJSON Image
instance FromJSON ImageEvent


-- Derive Defaults
instance Default Image where
  def = Image
    { _imageHash = mempty
    , _imagePath = mempty
    , _imageTags = mempty
    }


-- Provide a Projection
instance Project Image where
  foldEvent image = \case
    HashChanged v -> image { _imageHash = v }
    PathChanged v -> image { _imagePath = v }
    TagAppended v -> image { _imageTags = nub (_imageTags image <> [v]) }
    TagRemoved  v -> image { _imageTags = delete v (_imageTags image) }
