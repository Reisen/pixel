module API.User.Projections
  ( projectUsers
  , setupUserProjections
  ) where

import Protolude
import Text.InterpolatedString.QM
import Data.UUID                  ( UUID, toText )
import Database.SQLite.Simple     ( Query
                                  , Connection
                                  , execute_
                                  , execute
                                  , withTransaction
                                  )
import Pixel.Model.Users          ( User(..), Email(..), Password(..) )

--------------------------------------------------------------------------------

-- Ran on first run in order to setup table schema for projection, this also
-- writes any non-projected (but required data), such as standard roles. As
-- they are not event backed, they always exist and survive deletion after a
-- restart.
setupUserProjections
  :: MonadIO m
  => Connection
  -> m ()

setupUserProjections conn = liftIO $ withTransaction conn $ do
  -- Create Core Tables, and Default Data
  execute_ conn createUsersTable
  execute_ conn createRolesTable
  execute_ conn insertAdminRole
  execute_ conn insertStandardRole


projectUsers
  :: MonadIO m
  => Connection
  -> UUID
  -> User
  -> m ()

projectUsers conn uuid User{..} = liftIO $ withTransaction conn $ do
  putText $ fold ["[P:User] ", show _registeredAt, " ", show (_emailText <$> _email), ", Password: ", show _password]

  -- Insert Users
  execute conn insertUserRow
    ( toText uuid
    , _emailText <$> _email
    , _passwordText <$> _password
    , fromMaybe "" (toText <$> _role)
    , (show _state :: Text)
    , _registeredAt
    )

--------------------------------------------------------------------------------

createUsersTable :: Query
createUsersTable = [qns|
  CREATE TABLE IF NOT EXISTS users (
    uuid       TEXT     NOT NULL,
    username   TEXT         NULL,
    password   TEXT     NOT NULL,
    email      TEXT     NOT NULL,
    avatar     TEXT     NOT NULL,
    role       TEXT     NOT NULL,
    status     TEXT     NOT NULL,
    registered DATETIME NOT NULL,

    PRIMARY KEY (email),
    CHECK (status IN ('Banned', 'Verified', 'Unverified'))
  );
|]

createRolesTable :: Query
createRolesTable = [qns|
  CREATE TABLE IF NOT EXISTS roles (
    uuid        TEXT     NOT NULL,
    name        TEXT     NOT NULL,
    permissions TEXT     NOT NULL,
    PRIMARY KEY (uuid)
  );
|]

insertStandardRole :: Query
insertStandardRole = [qns|
  INSERT INTO roles      ( uuid, name, permissions )
  VALUES                 ( '099cd885-ae08-4610-8d14-16bd4af3a5a1'
                         , 'Standard'
                         , 'Image:create,Image:view,Image:edit'
                         )
  ON CONFLICT DO NOTHING;
|]

insertAdminRole :: Query
insertAdminRole = [qns|
  INSERT INTO roles      ( uuid, name, permissions )
  VALUES                 ( '6ae4e73a-f06a-4c0c-8111-73db7c6e323c'
                         , 'Admin'
                         , '*:*'
                         )
  ON CONFLICT DO NOTHING;
|]

insertUserRow :: Query
insertUserRow = [qns|
  INSERT INTO users ( uuid , email , password , avatar , role , status , registered )
  VALUES            ( ?    , ?     , ?        , ''     , ?    , ?      , ?          )
  ON CONFLICT DO NOTHING;
|]
