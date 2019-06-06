module Services.Static
  ( pixelWriteStaticImage
  )
where

--------------------------------------------------------------------------------

import Protolude
import Data.ByteString as B ( writeFile )

--------------------------------------------------------------------------------

-- Write static data to directory under the digest name.
pixelWriteStaticImage
  :: MonadIO m
  => Text
  -> Text
  -> ByteString
  -> m ()

pixelWriteStaticImage directory digest content =
  liftIO
    $ flip B.writeFile content
    $ fold [toS directory , "/" , toS digest]
