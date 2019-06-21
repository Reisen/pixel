module Server
  ( API
  , server
  ) where

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
import Pixel.API                            ( API )

import qualified API.Image.Routes           as Routes
import qualified API.User.Routes            as Routes
import qualified Network.HTTP.Types.Method  as Method

--------------------------------------------------------------------------------

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
      :<|> Routes.postUpdatePassword
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
