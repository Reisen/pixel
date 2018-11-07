module API
  ( server
  ) where

import Protolude
import Servant
import Imageless     (runImageless)
import Configuration (Configuration, Imageless)
import API.Image
  ( PostImage      , postImage
  , GetImage       , getImage
  , GetImageByUUID , getImageByUUID
  )


type API =
  ( "api" :>
    ( "image" :>
         PostImage
    :<|> GetImage
    :<|> GetImageByUUID
    )
  )


proxyAPI :: Proxy API
proxyAPI = Proxy



implAPI :: ServerT API Imageless
implAPI =
       postImage
  :<|> getImage
  :<|> getImageByUUID


server
  :: Configuration
  -> Application

server config =
  serve proxyAPI
    (hoistServer proxyAPI
      (runImageless config)
      (implAPI))
