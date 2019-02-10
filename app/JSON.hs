module JSON where

--------------------------------------------------------------------------------

import           Protolude

import qualified Data.Aeson                    as A
import qualified Data.Char                     as C

--------------------------------------------------------------------------------

pixelJsonEncoding :: A.Options
pixelJsonEncoding = A.defaultOptions
  { A.fieldLabelModifier =
      map C.toLower
        . (   length
          .   dropWhile C.isUpper
          .   dropWhile C.isLower
          .   reverse
          >>= drop
          )
  }
