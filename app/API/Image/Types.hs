module API.Image.Types
  ( HasHash(..)
  , HasTags(..)
  , HasUploader(..)
  , Image(..)
  , ImageEvent(..)
  ) where

import           Protolude               hiding ( hash )
import           Control.Lens
import           Data.Aeson                     ( FromJSON, ToJSON )
import           Data.Data                      ( Data )
import           Data.Default.Class             ( Default, def )
import           Data.List                      ( nub, delete )
import           Data.UUID                      ( UUID )
import           Eventless                      ( Project(..), Events )

--------------------------------------------------------------------------------

-- Define core Image datatype, this defines our representation of any uploaded
-- image when represented in our backend.
data Image = Image
  { _imageHash     :: Text
  , _imageTags     :: [Text]
  , _imageUploader :: Maybe UUID
  }
  deriving (Show, Generic, Typeable, Data)


-- Define an ImageEvent, so we can emit and project events that happen against
-- an Image in our backend.
data ImageEvent
  = HashChanged Text
  | TagAppended Text
  | TagRemoved Text
  | AssociatedWithUser UUID
  deriving (Show, Generic, Typeable, Data)


-- Derive Lenses & JSON
makeFields ''Image
makePrisms ''Image

instance ToJSON Image where
instance ToJSON ImageEvent where
instance FromJSON Image where
instance FromJSON ImageEvent where


-- Associate the Events for Images to be used during projection.
type instance Events Image = ImageEvent


-- Define a Default for it, this is required so we can do persistance with
-- Eventless/Event Sourcing.
instance Default Image where
  def = Image
    { _imageHash     = mempty
    , _imageTags     = mempty
    , _imageUploader = Nothing
    }


-- Provide an Eventless Projection for saving into the backend.
instance Project Image where
  foldEvent image = \case
    HashChanged v        -> image & hash     .~ v
    TagAppended v        -> image & tags     .~ nub (_imageTags image <> [v])
    TagRemoved  v        -> image & tags     .~ delete v (_imageTags image)
    AssociatedWithUser v -> image & uploader ?~ v
