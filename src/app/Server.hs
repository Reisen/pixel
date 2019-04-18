module Server
  ( server
  ) where

--------------------------------------------------------------------------------

import Protolude
import Servant

import API.Image.Routes                     ( PostImage
                                            , GetImage
                                            , GetImageByUUID
                                            , GetTags
                                            , PostTags
                                            , DeleteTags
                                            , getImage
                                            , getImageByUUID
                                            , getTags
                                            , postDeleteTags
                                            , postImage
                                            , postTags
                                            )
import Configuration                        ( Config )
import Network.Wai.Middleware.Cors          ( cors
                                            , simpleCorsResourcePolicy
                                            , corsRequestHeaders
                                            )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import MonadPixel                           ( Pixel, runPixel )

--------------------------------------------------------------------------------

-- Define a Servant API Type
--
-- By using this global API type, we can generate a whole bunch of things
-- automagically, including: an actual server for it, a client, a JS binding, a
-- swagger doc, a mock, etc.

type ImageAPI =
  "image"
    :> (    PostImage       -- POST    /image/
       :<|> GetImage        -- GET     /image/
       :<|> GetImageByUUID  -- GET     /image/:uuid
       :<|> GetTags         -- GET     /image/:uuid/tags
       :<|> PostTags        -- POST    /image/:uuid/tags
       :<|> DeleteTags      -- DELETE  /image/:uuid/tags
       )

type API = "api" :> ImageAPI


-- Proxy for Servant, Ignore.
proxyAPI :: Proxy API
proxyAPI = Proxy


-- Wrap up our actual methods, here we have to chain our methods in the same
-- order our API type above expects them to be in.
implAPI :: ServerT API Pixel
implAPI =
  postImage
    :<|> getImage
    :<|> getImageByUUID
    :<|> getTags
    :<|> postTags
    :<|> postDeleteTags

--------------------------------------------------------------------------------

-- Create a Servant Server to run in WAI.
server :: Config -> Application
server config =
  logStdout
    $ corsHandler
    $ serve proxyAPI
    $ hoistServer proxyAPI (runPixel config) implAPI

  where
    corsHandler =
      cors . const $ Just $ simpleCorsResourcePolicy
        { corsRequestHeaders =
          [ "Content-Type"
          , "Authorization"
          ]
        }
