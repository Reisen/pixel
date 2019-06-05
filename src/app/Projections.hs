module Projections
  ( handleProjections
  , prepareProjections
  ) where

import Protolude
import Control.Lens
import API.Image.Projections  ( setupImageProjections, projectImages )
import API.User.Projections   ( setupUserProjections, projectUsers )
import Data.Aeson             ( FromJSON, eitherDecode )
import Data.UUID              ( UUID )
import Database.SQLite.Simple ( Connection )
import Eventless              ( Event (..), kind )

--------------------------------------------------------------------------------

prepareProjections
  :: MonadIO m
  => Connection
  -> m ()

prepareProjections conn = do
  setupImageProjections conn
  setupUserProjections conn


handleProjections
  :: MonadIO m
  => Traversable t
  => Connection
  -> UUID
  -> t Event
  -> m ()

handleProjections conn uuid events = do
  case getLast $ foldMapDefault (Last . Just) events of
    Nothing    -> pure ()
    Just event -> case event ^. kind of
      "Image" -> overDecoded event $ projectImages conn uuid
      "User"  -> overDecoded event $ projectUsers conn uuid
      _       -> pure ()

--------------------------------------------------------------------------------

overDecoded
  :: Applicative f
  => FromJSON a
  => Event
  -> (a -> f ())
  -> f ()

overDecoded Event {..} f =
  (const (pure ()) `either` f)
    . eitherDecode
    . toS
    $ eventSnapshot
