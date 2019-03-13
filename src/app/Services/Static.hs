module Services.Static
  ( pixelWriteStaticImage
  )
where

--------------------------------------------------------------------------------

import           Protolude

import qualified Data.ByteString               as B
import qualified Pixel                         as Pixel

--------------------------------------------------------------------------------

-- Write static data to directory under the digest name.
pixelWriteStaticImage
  :: MonadIO m
  => Text
  -> Pixel.DigestText
  -> ByteString
  -> m ()

pixelWriteStaticImage directory digest content =
  liftIO
    $ flip B.writeFile content
    $ fold [toS directory , "/" , toS digest]
