module API.Image.Routes
  ( I.GetImage
  , I.GetImageByUUID
  , I.PostImage
  , I.GetTags
  , I.PostTags
  , I.DeleteTags
  , I.getImage
  , I.getImageByUUID
  , I.postImage
  , I.getTags
  , I.postTags
  , I.deleteTags
  )
where

--------------------------------------------------------------------------------

import qualified API.Image.Routes.GetImage       as I
import qualified API.Image.Routes.GetImageByUUID as I
import qualified API.Image.Routes.PostImage      as I
import qualified API.Image.Routes.GetTags        as I
import qualified API.Image.Routes.PostTags       as I
import qualified API.Image.Routes.DeleteTags     as I
