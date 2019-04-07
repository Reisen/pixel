module Projections where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens

import qualified API.Image.Projections         as API
import qualified Data.Aeson                    as A
import qualified Data.UUID                     as U
import qualified Database.SQLite.Simple        as S
import qualified Eventless                     as E

--------------------------------------------------------------------------------

handleProjections
  :: MonadIO m
  => Traversable t
  => S.Connection
  -> U.UUID
  -> t E.Event
  -> m ()

handleProjections conn uuid events = do
  case getLast $ foldMapDefault (Last . Just) events of
    Nothing    -> pure ()
    Just event -> case event ^. E.kind of
      "Image" -> overDecoded event $ API.projectImages conn uuid
      _       -> pure ()

--------------------------------------------------------------------------------

overDecoded
  :: Applicative f
  => A.FromJSON a
  => E.Event
  -> (a -> f ())
  -> f ()

overDecoded E.Event {..} f =
  (const (pure ()) `either` f)
    . A.eitherDecode
    . toS
    $ eventSnapshot
