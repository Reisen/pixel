module API.Image.Endpoints.PostImage
  ( PostImage
  , PostImageRequest(..)
  , HasPath(..)
  , HasTags(..)
  )
where

import           Protolude
import           Control.Lens
import           Data.Aeson                     ( FromJSON, ToJSON )
import           Servant
import           Servant.Multipart
import           System.IO                      ( FilePath )


-- We receive incoming images as a multi-part form so we can receive file data
-- from the sender.
type PostImage
  =  MultipartForm Tmp PostImageRequest
  :> Post '[JSON] NoContent


-- Define a type to wrap up the MultipartData coming over the wire.
data PostImageRequest = PostImageRequest
  { postImageRequestPath :: !FilePath
  , postImageRequestTags :: ![Text]
  } deriving (Show, Generic)


data PostImageResponse = PostImageResponse
  { postImageResponseUUID :: !Text
  , postImageResponseHash :: !Text
  } deriving (Show, Generic)


-- Provide an implementation to parse incoming data into our type above.
instance FromMultipart Tmp PostImageRequest where
  fromMultipart multi =
    let allInputs = inputs multi in
    let tagInputs = flip filter allInputs $ (== "tag") . iName in
    let allValues = iValue <$> tagInputs in
    PostImageRequest
      <$> map fdPayload (lookupFile "image" multi)
      <*> pure allValues


-- Generate Lenses & JSON
makeFields ''PostImageRequest
instance ToJSON PostImageRequest where
instance FromJSON PostImageResponse where
instance FromJSON PostImageRequest where
instance ToJSON PostImageResponse where
