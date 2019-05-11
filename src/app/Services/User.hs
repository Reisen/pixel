module Services.User
  ( pixelCreateUser
  , pixelFindRoleByName
  , pixelFindUserByUUID
  , pixelFindUserByEmail
  ) where

import Protolude
import Control.Lens
import Text.InterpolatedString.QM

import API.User.Commands      ( createUser )
import Configuration          ( Config, configConnection, configReadSchema )
import Data.Default.Class     ( def )
import Data.Text              ( splitOn )
import Data.Time              ( UTCTime )
import Data.UUID              ( UUID, toText, fromText )
import Database.SQLite.Simple ( Only(..), query )
import Eventless              ( runCommand )
import Pixel.API.Users        ( User(..), Role(..), Email(..), Password(..), makePermission )

--------------------------------------------------------------------------------

pixelCreateUser
  :: MonadIO m
  => MonadReader Config m
  => UUID
  -> UUID
  -> UTCTime
  -> Email
  -> Password
  -> m ()

pixelCreateUser uuid roleUUID createdAt email password = do
  backend <- view configConnection
  let create = createUser roleUUID email password createdAt
  void $ runCommand backend uuid create


pixelFindRoleByName
  :: MonadIO m
  => MonadReader Config m
  => Text
  -> m (Maybe (UUID, Role))

pixelFindRoleByName name = do
  schema  <- view configReadSchema
  mayRole <- liftIO $ flip (query schema) (Only name) [qns|
    SELECT  r.uuid, r.permissions
    FROM    roles r
    WHERE   name = ?
  |]

  pure $ case head mayRole of
    Nothing                     -> Nothing
    Just (mayUUID, permissions) -> case fromText mayUUID of
      Nothing   -> Nothing
      Just uuid -> Just
        ( uuid
        , Role
          { _roleName        = name
          , _rolePermissions = catMaybes $ makePermission <$> splitOn "," permissions
          , _roleDeletedAt   = Nothing
          }
        )


pixelFindUserByUUID
  :: MonadIO m
  => MonadReader Config m
  => UUID
  -> m (Maybe User)

pixelFindUserByUUID userUUID = do
  schema  <- view configReadSchema
  mayUser <- liftIO $ flip (query schema) (Only $ toText userUUID) [qns|
    SELECT  u.username, u.email, u.password
    FROM    users u
    WHERE   u.uuid = ?
  |]

  pure $ case head mayUser of
    _                                -> Nothing
    Just (username, email, password) ->
      Just $ def
        { _userUsername = username
        , _userEmail    = Just (Email email)
        , _userPassword = Just (UnsafeMkPassword_ password)
        }


pixelFindUserByEmail
  :: MonadIO m
  => MonadReader Config m
  => Email
  -> m (Maybe (UUID, User))

pixelFindUserByEmail (Email email) = do
  schema  <- view configReadSchema
  mayUser <- traceShowId <$> (liftIO $ flip (query schema) (Only $ traceShowId email) [qns|
    SELECT  u.uuid, u.username, u.password
    FROM    users u
    WHERE   u.email = ?
  |])

  pure $ case head mayUser of
    Nothing                            -> Nothing
    Just (mayUUID, username, password) ->
      case fromText mayUUID of
        Nothing   -> Nothing
        Just uuid -> Just
          ( uuid
          , def
            { _userUsername = username
            , _userEmail    = Just (Email email)
            , _userPassword = Just (UnsafeMkPassword_ password)
            }
          )
