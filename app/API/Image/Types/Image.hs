module API.Image.Types.Image
  ( Image(..)
  , HasHash(..)
  , HasPath(..)
  , HasTags(..)
  )
where

import           Protolude               hiding ( hash )
import           API.Image.Types.ImageEvent     ( ImageEvent(..) )
import           Control.Lens
import           Data.Aeson                     ( FromJSON, ToJSON )
import           Data.Data                      ( Data )
import           Data.Default.Class             ( Default, def )
import           Data.List                      ( nub, delete )
import           Eventless                      ( Project(..), Events )


-- Define core Image datatype, this defines our representation of any uploaded
-- image when represented in our backend.
data Image = Image
  { _imageHash :: Text
  , _imagePath :: Text
  , _imageTags :: [Text]
  }
  deriving (Show, Generic, Typeable, Data)


-- Derive Lenses for it.
makeFields ''Image


-- Derive JSON for it.
instance ToJSON Image where
instance FromJSON Image where


-- Define a Default for it.
instance Default Image where
  def = Image
    { _imageHash = mempty
    , _imagePath = mempty
    , _imageTags = mempty
    }


-- Provide a Projection to actually apply this event to an Image.
instance Project Image where
  foldEvent image = \case
    HashChanged v -> image { _imageHash = v }
    PathChanged v -> image { _imagePath = v }
    TagAppended v -> image { _imageTags = nub (_imageTags image <> [v]) }
    TagRemoved  v -> image { _imageTags = delete v (_imageTags image) }


-- Associate the Event with the Image.
type instance Events Image = ImageEvent
