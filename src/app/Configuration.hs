module Configuration
  ( Config'(..)
  , Config
  , configConnection
  , configPort
  , configReadSchema
  , configStaticLocation
  , readConfig
  , readNumericEnv
  , readTextEnv
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens

import           Pixel                          ( HKD )
import qualified Database.SQLite.Simple        as S
import qualified Eventless                     as Ev
import qualified System.Environment            as S.E
import qualified Text.Read                     as T.R

--------------------------------------------------------------------------------

-- HKD Configuration Definition
data Config' f = Config
  { _configStaticLocation :: HKD f Text
  , _configPort           :: HKD f Int
  , _configConnection     :: HKD f Ev.BackendStore
  , _configReadSchema     :: HKD f S.Connection
  }

-- Concrete Configuration
type Config = Config' Identity

-- Derivations
makeLenses ''Config'

--------------------------------------------------------------------------------

-- Read configuration into a concrete value from withing some functor. Similar
-- to using traverse_ for types.
readConfig :: Config' IO -> IO Config
readConfig Config {..} =
  Config
    <$> _configStaticLocation
    <*> _configPort
    <*> _configConnection
    <*> _configReadSchema

-- Environment Reading Helpers
--
-- These are intentionally partial, crashing on failing to read the Environment
-- is pretty much fine as the exception thrown is useful and it is before the
-- application has started doing anything meaningful.
readTextEnv :: Text -> Text ->  IO Text
readTextEnv key def =
  fromMaybe def . map toS
    <$> S.E.lookupEnv (toS key)

readNumericEnv :: Text -> Int -> IO Int
readNumericEnv key def =
  fromMaybe def . map (T.R.read . toS)
    <$> S.E.lookupEnv (toS key)
