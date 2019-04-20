module Pixel.API.Images.Operations.PostImage
  ( ImageDetails(..)
  , digestImage
  , handleImageUpload
  , processImage
  ) where

import Protolude hiding       ( hash )
import Crypto.Hash            ( hash, Digest, SHA3_224 )
import Data.UUID              ( UUID, fromText )
import Data.Time              ( UTCTime )
import Pixel.API.Token        ( Token (..) )
import Pixel.API.Images.Types ( Image (..), TagList )
import Pixel.Services.Image   ( MonadImage (..) )
import Pixel.Services.Static  ( MonadStatic (..) )

--------------------------------------------------------------------------------

-- | Given a block of bytes, produce a stored image under the hashname of those
-- | bytes. The result of this function is just the hash of the newly created
-- | file within the backend.
handleImageUpload
  :: Monad m       -- ^ Any Monad...
  => MonadStatic m -- ^ ... that can store static data
  => MonadImage m  -- ^ ... that can work with Image data
  => ImageDetails  -- ^ The Image we want to upload.
  -> m ()

handleImageUpload ImageDetails {..} = do
  let digest = digestImage content
  let image  = processImage createdAt uploadedTags token content
  writeStaticImage directory digest content
  saveImage uuid image
  pure ()

--------------------------------------------------------------------------------

-- | Given a block of bytes, what is the hash we should extract? If we ever
-- | change this it will become possible to upload multiple identical images
-- | using a different hash. This may be desireable?
digestImage :: ByteString -> Text
digestImage = show . (hash :: ByteString -> Digest SHA3_224)

-- Given all uploaded data, produce a valid API image type.
processImage :: UTCTime -> [Text] -> Token -> ByteString -> Image
processImage createdAt newTags token content = Image
  { _imageHash      = digestImage content
  , _imageTags      = newTags
  , _imageUploader  = fromText . _tokenText $ token
  , _imageCreatedAt = Just createdAt
  , _imageDeletedAt = Nothing
  }

--------------------------------------------------------------------------------

data ImageDetails = ImageDetails
  { content      :: ByteString -- ^ Binary content of the Image to store.
  , createdAt    :: UTCTime    -- ^ Time of Upload
  , directory    :: Text       -- ^ Where do we store this image?
  , token        :: Token      -- ^ Token for user doing this request.
  , uploadedTags :: TagList    -- ^ Tags to associate with this image.
  , uuid         :: UUID       -- ^ What UUID do we assign to this resouce?
  }
