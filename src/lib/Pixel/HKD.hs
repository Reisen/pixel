module Pixel.HKD ( HKD ) where

--------------------------------------------------------------------------------

import           Protolude

--------------------------------------------------------------------------------

-- Erase Identity HKD types so we can restore HKD to normal concrete data-types
-- when needed.
type family HKD (f :: * -> *) a where
  HKD Identity a = a
  HKD f        a = f a
