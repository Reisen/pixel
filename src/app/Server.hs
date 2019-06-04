module Server
  ( API
  , server
  ) where

--------------------------------------------------------------------------------

import Protolude
import Servant
import Pixel.API.AppendImageTags  as AppendImageTags
import Pixel.API.CreateImage      as CreateImage
-- import Pixel.API.DeleteTags       as DeleteTags
import Pixel.API.FetchImageByUUID as FetchImageByUUID
import Pixel.API.FetchImageTags   as FetchImageTags
import Pixel.API.FetchImages      as FetchImages

import Configuration                        ( Config, Config'(..) )
import Network.HTTP.Types.Method          as Method
import Network.Wai.Middleware.Cors          ( cors
                                            , simpleCorsResourcePolicy
                                            , corsRequestHeaders
                                            , corsMethods
                                            , corsOrigins
                                            )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import MonadPixel                           ( Pixel, runPixel )

-- Import Routes
import API.Image.Routes                     ( DeleteTags
                                            , getImage
                                            , getImageByUUID
                                            , getTags
                                            , postDeleteTags
                                            , postImage
                                            , postTags
                                            )
import API.User.Routes                      ( AuthenticateUser
                                            , RegisterUser
                                            , postAuthenticateUser
                                            , postRegisterUser
                                            )

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
    :<|> AppendImageTags.Route -- POST    /image/:uuid/tags
    :<|> DeleteTags            -- DELETE  /image/:uuid/tags
    )

type UserAPI =
  "user" :>
    (    AuthenticateUser -- POST /user/login
    :<|> RegisterUser     -- POST /user
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
      (    postImage
      :<|> getImage
      :<|> getImageByUUID
      :<|> getTags
      :<|> postTags
      :<|> postDeleteTags
      )

    userAPI =
      (    postAuthenticateUser
      :<|> postRegisterUser
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
