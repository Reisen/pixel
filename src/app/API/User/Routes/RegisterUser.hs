module API.User.Routes.RegisterUser
  ( RegisterUser
  , RegisterRequest
  , postRegisterUser
  ) where

import Protolude hiding ( hash )
import Servant

import Crypto.Random    ( getRandomBytes )
import Crypto.Hash      ( Digest, SHA3_224(..), hash )
import Data.Aeson       ( FromJSON(..) )
import Data.UUID        ( toText )
import Data.UUID.V4     ( nextRandom )
import Data.Time        ( getCurrentTime )
import Pixel            ( pixelParseJSON )
import Pixel.API.Token  ( Token(..) )
import Pixel.API.Users  ( RegisterDetails(..)
                        , handleRegisterUser
                        )
import MonadPixel       ( Pixel )

--------------------------------------------------------------------------------

type RegisterUser =
  ReqBody '[JSON] RegisterRequest
    :> Post '[JSON] Token

--------------------------------------------------------------------------------

data RegisterRequest = RegisterRequest
  { _registerRequestEmail    :: Text
  , _registerRequestPassword :: Text
  } deriving (Show, Generic)

instance FromJSON RegisterRequest where
  parseJSON = pixelParseJSON

--------------------------------------------------------------------------------

postRegisterUser
  :: RegisterRequest
  -> Pixel Token

postRegisterUser RegisterRequest{..} = do
  uuid                   <- liftIO nextRandom
  createdAt              <- liftIO getCurrentTime
  hashSalt :: ByteString <- liftIO $ getRandomBytes 8
  handleRegisterUser RegisterDetails
    { _rdUUID      = uuid
    , _rdCreatedAt = createdAt
    , _rdEmail     = _registerRequestEmail
    , _rdPassword  = _registerRequestPassword
    , _rdSalt      = show (hash hashSalt :: Digest SHA3_224)
    }

  pure . Token . toText $ uuid
