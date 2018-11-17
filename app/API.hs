module API
  ( server
  )
where

import           Protolude
import           Servant
import           Imageless                      ( runImageless )
import           Configuration                  ( Configuration, Imageless )
import           API.Image.Endpoints            ( GetImage
                                                , GetImageByUUID
                                                , PostImage
                                                , getImage
                                                , getImageByUUID
                                                , postImage
                                                )


-- Define a Servant API Type
--
-- By using this global API type, we can generate a whole bunch of things
-- automagically, including: an actual server for it, a client, a JS binding, a
-- swagger doc, a mock, etc.
type API =
  ( "api" :>
    ( "image" :>
         PostImage
    :<|> GetImage
    :<|> GetImageByUUID
    )
  )


-- Proxy for Servant, Ignore.
proxyAPI :: Proxy API
proxyAPI = Proxy



-- Wrap up our actual methods, here we have to chain our methods in the same
-- order our API type above expects them to be in.
implAPI :: ServerT API Imageless
implAPI =
       postImage
  :<|> getImage
  :<|> getImageByUUID


-- Create a Servant Server to run in WAI.
server
  :: Configuration
  -> Application

server config =
  serve proxyAPI
    $ hoistServer proxyAPI (runImageless config) implAPI
