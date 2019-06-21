module API.User.Routes.UpdatePasswordUser
  (
      postUpdatePassword
  ) where

import Protolude hiding             ( hash )
import Servant
import Data.UUID                    ( fromText )
import Crypto.Random                ( getRandomBytes )
import Crypto.Hash                  ( Digest, SHA3_224(..), hash )
import MonadPixel                   ( Pixel )
import Pixel                        ( Error(..), CookieToken(..) )
import Pixel.Model.Token            ( _tokenText )
import Pixel.API.UpdatePasswordUser ( Request(..) )
import Pixel.Operations             ( UpdatePasswordDetails(..)
                                    , updateUserPassword
                                    )
--------------------------------------------------------------------------------

postUpdatePassword
  :: Maybe CookieToken
  -> Request
  -> Pixel NoContent

postUpdatePassword Nothing _ = throwError UnknownError
postUpdatePassword (Just (CookieToken token)) Request{..} = do
  hashSalt :: ByteString <- liftIO $ getRandomBytes 16
  let mayUUID = fromText . _tokenText $ token
  case mayUUID of
    Just uuid -> do
      updateUserPassword UpdatePasswordDetails 
        {
          _uuid            = uuid
        , _currentPassword = _currentPassword
        , _newPassword     = _newPassword
        , _salt            = show (hash hashSalt :: Digest SHA3_224)
        }
      pure NoContent
    Nothing -> pure NoContent