module Error
  ( PixelError(..)
  )
where

--------------------------------------------------------------------------------

import           Protolude

import qualified API.Image.Error as E

--------------------------------------------------------------------------------

data PixelError
  = ImageError E.ImageError
  | OtherError
  deriving Show
