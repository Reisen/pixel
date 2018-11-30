module API.Image.Types.ImageEvent
  ( ImageEvent(..)
  )
where

import           Protolude
import           Data.Aeson                     ( FromJSON, ToJSON )
import           Data.Data                      ( Data )
import           Data.UUID                      ( UUID )


-- Define an ImageEvent, so we can emit and project events that happen against
-- an Image in our backend.
data ImageEvent
  = HashChanged Text
  | TagAppended Text
  | TagRemoved Text
  | AssociatedWithUser UUID
  deriving (Show, Generic, Typeable, Data)


-- Derive JSON for it.
instance ToJSON ImageEvent where
instance FromJSON ImageEvent where
