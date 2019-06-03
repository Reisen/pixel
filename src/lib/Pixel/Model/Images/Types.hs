module Pixel.Model.Images.Types
  ( DigestText
  , Image(..)
  , ImageEvent(..)
  , TagList
  ) where

import Protolude
import Data.Aeson         ( ToJSON (..), FromJSON (..) )
import Data.Data          ( Data )
import Data.Default.Class ( Default, def )
import Data.List          ( nub, delete )
import Data.Time          ( UTCTime )
import Data.UUID          ( UUID )
import Eventless          ( Events, Project (..) )

--------------------------------------------------------------------------------

-- Useful Tag Alises for working with Images
type TagList    = [Text]
type DigestText = Text

--------------------------------------------------------------------------------

-- Define core Image datatype, this defines our representation of any uploaded
-- image when represented in our backend.

data Image = Image
  { _createdAt :: !(Maybe UTCTime)
  , _deletedAt :: !(Maybe UTCTime)
  , _hash      :: !Text
  , _tags      :: ![Text]
  , _uploader  :: !(Maybe UUID)
  } deriving (Show, Generic, Typeable, Data)

--------------------------------------------------------------------------------

-- Define an ImageEvent, so we can emit and project events that happen against
-- an Image in our backend.

data ImageEvent
  = AssociatedWithUser !UUID
  | CreatedAt !UTCTime
  | DeletedAt !UTCTime
  | HashChanged !Text
  | TagAppended !Text
  | TagRemoved !Text
  deriving (Show, Generic, Typeable, Data)

type instance Events Image = ImageEvent

--------------------------------------------------------------------------------

instance ToJSON Image where
instance ToJSON ImageEvent where
instance FromJSON Image where
instance FromJSON ImageEvent where

--------------------------------------------------------------------------------

instance Default Image where
  def = Image
    { _hash      = mempty
    , _tags      = mempty
    , _uploader  = Nothing
    , _createdAt = Nothing
    , _deletedAt = Nothing
    }

--------------------------------------------------------------------------------

-- Provide an Eventless Projection for saving into the backend.
instance Project Image where
  foldEvent image = \case
    HashChanged v        -> image { _hash      = v }
    TagAppended v        -> image { _tags      = nub (_tags image <> [v]) }
    TagRemoved  v        -> image { _tags      = delete v (_tags image) }
    AssociatedWithUser v -> image { _uploader  = Just v }
    CreatedAt v          -> image { _createdAt = Just v }
    DeletedAt v          -> image { _deletedAt = Just v }
