module API.Image.Types.Endpoints
  ( PostImage
  , GetImage
  , GetImageByUUID
  )
where

import           Protolude
import           Servant                        ( NoContent(..)
                                                , (:>)
                                                , Get
                                                , Post
                                                , JSON
                                                , Capture
                                                , ReqBody
                                                )


type PostImage
  =  MultiPartBody
  :> ReqBody '[JSON] ReqImage
  :> Post '[JSON] NoContent


type GetImage
  = Get '[JSON] [ReqImage]


type GetImageByUUID
  =  Capture "uuid" Text
  :> Get '[JSON] ReqImage
