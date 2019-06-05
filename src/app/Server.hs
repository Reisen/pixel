module Server
  ( API
  , server
  ) where

--------------------------------------------------------------------------------

import Protolude
import Servant

import Configuration                        ( Config, Config'(..) )
import Network.Wai.Middleware.Cors          ( cors
                                            , corsRequestHeaders
                                            , corsMethods
                                            , corsOrigins
                                            , simpleCorsResourcePolicy
                                            )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import MonadPixel                           ( Pixel, runPixel )

import qualified API.Image.Routes           as Routes
import qualified API.User.Routes            as Routes
import qualified Network.HTTP.Types.Method  as Method
import qualified Pixel.API.AppendImageTags  as AppendImageTags
import qualified Pixel.API.CreateImage      as CreateImage
import qualified Pixel.API.DeleteImageTags  as DeleteImageTags
import qualified Pixel.API.FetchImageByUUID as FetchImageByUUID
import qualified Pixel.API.FetchImageTags   as FetchImageTags
import qualified Pixel.API.FetchImages      as FetchImages


--------------------------------------------------------------------------------

-- Define a Servant API Type
--
-- By using this global API type, we can generate a whole bunch of things
-- automagically, including: an actual server for it, a client, a JS binding, a
-- swagger doc, a mock, etc.

type ImageAPI =
  "image" :>
    (    CreateImage.Route      -- POST    /image/
    :<|> FetchImages.Route      -- GET     /image/
    :<|> FetchImageByUUID.Route -- GET     /image/:uuid
    :<|> FetchImageTags.Route   -- GET     /image/:uuid/tags
    :<|> AppendImageTags.Route  -- POST    /image/:uuid/tags
    :<|> DeleteImageTags.Route  -- DELETE  /image/:uuid/tags
    )

type UserAPI =
  "user" :>
    (    Routes.AuthenticateUser -- POST /user/login
    :<|> Routes.RegisterUser     -- POST /user
    )

type API =
  "api" :>
    (    ImageAPI
    :<|> UserAPI
    )


-- Proxy for Servant, Ignore.
proxyAPI :: Proxy API
proxyAPI = Proxy


-- Wrap up our actual methods, here we have to chain our methods in the same
-- order our API type above expects them to be in.
implAPI :: ServerT API Pixel
implAPI =
  (    imageAPI
  :<|> userAPI
  )

  where
    imageAPI =
      (    Routes.postImage
      :<|> Routes.getImage
      :<|> Routes.getImageByUUID
      :<|> Routes.getTags
      :<|> Routes.postTags
      :<|> Routes.postDeleteTags
      )

    userAPI =
      (    Routes.postAuthenticateUser
      :<|> Routes.postRegisterUser
      )

--------------------------------------------------------------------------------

-- Create a Servant Server to run in WAI.
server :: Config -> Application
server config@Config{..} =
  logStdout
    $ corsHandler
    $ serve proxyAPI
    $ hoistServer proxyAPI (runPixel config) implAPI

  where
    corsHandler =
      cors . const $ Just $ simpleCorsResourcePolicy
        { corsRequestHeaders =
            [ "Content-Type"
            ]

        , corsOrigins = Just
            ( [toS _configClientAddress], True )

        , corsMethods =
            [ Method.methodGet
            , Method.methodPost
            , Method.methodHead
            , Method.methodDelete
            ]
        }
