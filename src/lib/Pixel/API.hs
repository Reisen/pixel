module Pixel.API
  ( module API
  , API
  ) where

import Servant
import Pixel.API.Types                      as API

import qualified Pixel.API.AppendImageTags  as AppendImageTags
import qualified Pixel.API.AuthenticateUser as AuthenticateUser
import qualified Pixel.API.CreateImage      as CreateImage
import qualified Pixel.API.DeleteImageTags  as DeleteImageTags
import qualified Pixel.API.FetchImageByUUID as FetchImageByUUID
import qualified Pixel.API.FetchImageTags   as FetchImageTags
import qualified Pixel.API.FetchImages      as FetchImages
import qualified Pixel.API.RegisterUser     as RegisterUser

--------------------------------------------------------------------------------

-- Define a Servant API Type
--
-- By using this global API type, we can generate a whole bunch of things
-- automagically, including: an actual server for it, a client, a JS binding,
-- a swagger doc, a mock, etc.

type ImageAPI =
  "image" :>
    (    CreateImage.Route       -- POST    /image/
    :<|> FetchImages.Route       -- GET     /image/
    :<|> FetchImageByUUID.Route  -- GET     /image/:uuid
    :<|> FetchImageTags.Route    -- GET     /image/:uuid/tags
    :<|> AppendImageTags.Route   -- POST    /image/:uuid/tags
    :<|> DeleteImageTags.Route   -- DELETE  /image/:uuid/tags
    )

type UserAPI =
  "user" :>
    (    AuthenticateUser.Route  -- POST /user/login
    :<|> RegisterUser.Route      -- POST /user
    )

type API =
  "api" :>
    (    ImageAPI
    :<|> UserAPI
    )
