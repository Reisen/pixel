module Pixel.API.Images.Routes.PostImage
  ( ImageDetails(..)
  , digestImage
  , handleImageUpload
  , processImage
  )
where

--------------------------------------------------------------------------------

import           Protolude

import qualified Crypto.Hash                   as H
import qualified Data.UUID                     as U
import           Data.Time                      ( UTCTime )
import qualified Pixel.API.Token               as Pixel
import qualified Pixel.API.Images.Types        as Pixel
import qualified Pixel.Services.Image          as Pixel
import qualified Pixel.Services.Static         as Pixel

--------------------------------------------------------------------------------

-- | Given a block of bytes, produce a stored image under the hashname of those
-- | bytes. The result of this function is just the hash of the newly created
-- | file within the backend.
handleImageUpload
  :: Monad m             -- ^ Any Monad
  => Pixel.MonadStatic m -- ^ Access to Static API to store data.
  => Pixel.MonadImage m  -- ^ Access to Image API to write/read images.
  => ImageDetails        -- ^ Argument Itself
  -> m ()

handleImageUpload ImageDetails {..} = do
  let digest = digestImage content
  let image  = processImage createdAt uploadedTags token content
  Pixel.writeStaticImage directory digest content
  Pixel.saveImage uuid image
  pure ()

--------------------------------------------------------------------------------

-- | Given a block of bytes, what is the hash we should extract? If we ever
-- | change this it will become possible to upload multiple identical images
-- | using a different hash. This may be desireable?
digestImage :: ByteString -> Text
digestImage = show . (H.hash :: ByteString -> H.Digest H.SHA3_224)

-- Given all uploaded data, produce a valid API image type.
processImage :: UTCTime -> [Text] -> Pixel.Token -> ByteString -> Pixel.Image
processImage createdAt newTags token content = Pixel.Image
  { _imageHash      = digestImage content
  , _imageTags      = newTags
  , _imageUploader  = U.fromText . Pixel._tokenText $ token
  , _imageCreatedAt = Just createdAt
  }

--------------------------------------------------------------------------------

data ImageDetails = ImageDetails
  { content      :: ByteString    -- ^ Content of the Image Itself
  , createdAt    :: UTCTime       -- ^ Time of Upload
  , directory    :: Text          -- ^ Where do we store this image?
  , token        :: Pixel.Token   -- ^ Token for user doing this request.
  , uploadedTags :: Pixel.TagList -- ^ Tags to associate with this image.
  , uuid         :: U.UUID        -- ^ What UUID do we assign to this resouce?
  }
