module Services.Static
  ( pixelWriteStaticImage
  )
where

--------------------------------------------------------------------------------

import Protolude
import Data.ByteString as B ( writeFile )
import Pixel.API.Images     ( DigestText )

--------------------------------------------------------------------------------

-- Write static data to directory under the digest name.
pixelWriteStaticImage
  :: MonadIO m
  => Text
  -> DigestText
  -> ByteString
  -> m ()

pixelWriteStaticImage directory digest content =
  liftIO
    $ flip B.writeFile content
    $ fold [toS directory , "/" , toS digest]
