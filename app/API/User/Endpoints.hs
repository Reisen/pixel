module API.User.Endpoints
  () where
--   RegisterRequest
--   , PostUser
--   , GetUser
--   , GetUserByUUID
--   , postUser
--   , getUser
--   , getUserByUUID
--   ) where
--
-- import Protolude
-- import Servant
--   ( NoContent(..)
--   , (:>)
--   , Get
--   , Post
--   , JSON
--   , Capture
--   , ReqBody
--   )
--
--
-- data RegisterRequest = RegisterRequest
--   { registerRequestUsername :: Text
--   , registerRequestPassword :: Text
--   } deriving (Show, Generic)
--
-- makeFields ''RegisterRequest
--
-- instance FromJSON RegisterRequest
-- instance ToJSON RegisterRequest
--
--
-- data UserResponse = UserResponse
--   { userResponseUsername :: Text
--   } deriving (Show, Generic)
--
-- makeFields ''UserResponse
-- instance FromJSON UserResponse
-- instance ToJSON UserResponse
--
--
-- type PostUser
--   =  ReqBody '[JSON] RegisterRequest
--   :> Post    '[JSON] NoContent
--
-- postUser
--   :: MonadImageless m
--   => MonadReader config m
--   => HasConnection config BackendStore
--   => RegisterRequest
--   -> m NoContent
--
-- postUser request = do
--   validateUsername (request ^. username)
--   validatePassword (request ^. password)
--   uuid <- generateUUID
--   _    <- persistUser uuid request
--   pure NoContent
--
--
-- getUser
--   :: MonadImageless m
--   => m [UserResponse]
--
-- getUser = undefined
