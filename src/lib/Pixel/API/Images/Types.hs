module Pixel.API.Images.Types
  ( DigestText
  , Image(..)
  , ImageEvent(..)
  , TagList

  -- Lens
  , imageHash
  , imageTags
  , imageUploader
  , imageCreatedAt

  -- Prisms
  , _HashChanged
  , _TagAppended
  , _TagRemoved
  , _AssociatedWithUser
  ) where

import Protolude
import Control.Lens
import Data.Aeson           ( ToJSON (..), FromJSON (..) )
import Data.Data            ( Data )
import Data.Default.Class   ( Default, def )
import Data.List            ( nub, delete )
import Data.Time            ( UTCTime )
import Data.UUID            ( UUID )
import Eventless            ( Events, Project (..) )
import Pixel.JSON           ( pixelToJSON, pixelToEncoding, pixelParseJSON )

--------------------------------------------------------------------------------

-- Useful Tag Alises for working with Images
type TagList    = [Text]
type DigestText = Text

--------------------------------------------------------------------------------

-- Define core Image datatype, this defines our representation of any uploaded
-- image when represented in our backend.
data Image = Image
  { _imageCreatedAt :: Maybe UTCTime
  , _imageHash      :: Text
  , _imageTags      :: [Text]
  , _imageUploader  :: Maybe UUID
  } deriving (Show, Generic, Typeable, Data)

-- Derivations
instance ToJSON Image where
  toJSON     = pixelToJSON
  toEncoding = pixelToEncoding

instance FromJSON Image where
  parseJSON  = pixelParseJSON

makeLenses ''Image

--------------------------------------------------------------------------------

-- Define an ImageEvent, so we can emit and project events that happen against
-- an Image in our backend.
data ImageEvent
  = HashChanged !Text
  | TagAppended !Text
  | TagRemoved !Text
  | AssociatedWithUser !UUID
  | CreatedAt !UTCTime
  deriving (Show, Generic, Typeable, Data)

-- Derivations
instance ToJSON ImageEvent where
instance FromJSON ImageEvent where

makePrisms ''ImageEvent

--------------------------------------------------------------------------------

-- Associate the Events for Images to be used during projection.
type instance Events Image = ImageEvent

-- Define a Default for it, this is required so we can do persistance with
-- Eventless/Event Sourcing.
instance Default Image where
  def = Image
    { _imageHash      = mempty
    , _imageTags      = mempty
    , _imageUploader  = Nothing
    , _imageCreatedAt = Nothing
    }

-- Provide an Eventless Projection for saving into the backend.
instance Project Image where
  foldEvent image = \case
    HashChanged v        -> image { _imageHash      = v }
    TagAppended v        -> image { _imageTags      = nub (_imageTags image <> [v]) }
    TagRemoved  v        -> image { _imageTags      = delete v (_imageTags image) }
    AssociatedWithUser v -> image { _imageUploader  = Just v }
    CreatedAt v          -> image { _imageCreatedAt = Just v }
