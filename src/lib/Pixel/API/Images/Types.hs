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
  )
where

--------------------------------------------------------------------------------

import           Protolude               hiding ( hash )
import           Control.Lens
import qualified Data.Aeson                    as A
import           Data.Data                      ( Data )
import           Data.Default.Class             ( Default, def )
import           Data.List                      ( nub, delete )
import           Data.Time                      ( UTCTime )
import           Data.UUID                      ( UUID )
import qualified Eventless                     as E
import qualified Pixel.JSON                    as J

--------------------------------------------------------------------------------

-- Useful Tag Alises for working with Images
type TagList    = [Text]
type DigestText = Text

--------------------------------------------------------------------------------

-- Define core Image datatype, this defines our representation of any uploaded
-- image when represented in our backend.
data Image = Image
  { _imageHash      :: Text
  , _imageTags      :: [Text]
  , _imageUploader  :: Maybe UUID
  , _imageCreatedAt :: Maybe UTCTime
  } deriving (Show, Generic, Typeable, Data)

-- Derivations
instance A.ToJSON Image where
  toJSON     = J.pixelToJSON
  toEncoding = J.pixelToEncoding

instance A.FromJSON Image where
  parseJSON = J.pixelParseJSON

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
instance A.ToJSON ImageEvent where
instance A.FromJSON ImageEvent where

makePrisms ''ImageEvent

--------------------------------------------------------------------------------

-- Associate the Events for Images to be used during projection.
type instance E.Events Image = ImageEvent

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
instance E.Project Image where
  foldEvent image = \case
    HashChanged v        -> image { _imageHash      = v }
    TagAppended v        -> image { _imageTags      = nub (_imageTags image <> [v]) }
    TagRemoved  v        -> image { _imageTags      = delete v (_imageTags image) }
    AssociatedWithUser v -> image { _imageUploader  = Just v }
    CreatedAt v          -> image { _imageCreatedAt = Just v }
