module Configuration
  ( Configuration(..)
  , readTextEnv
  , readNumericEnv

  -- Lens
  , HasStaticLocation(..)
  , HasPort(..)
  , HasConnection(..)

  -- Monad Transformer
  , Imageless
  , MonadImageless
  )
where


import           Protolude
import           Control.Lens
import           Eventless                      ( BackendStore(..) )
import           Imageless                      ( MakeImageless, MakeMonadImageless )
import           System.Environment             ( getEnv )
import           Text.Read                      ( read )


-- See `Imageless.hs` for an explanation of why we use `Make` functions to
-- generate Monads here. Otherwise, these are your standard Monad and MTL
-- constraints.
type Imageless = MakeImageless () Configuration
type MonadImageless m = MakeMonadImageless () Configuration m


-- Configuration used as our MonadReader context for the application.
data Configuration = Configuration
  { _configurationStaticLocation :: Text
  , _configurationPort           :: Int
  , _configurationConnection     :: BackendStore
  }

makeFields ''Configuration


-- Environment Reading Helpers
-- These are intentionally partial, crashing on failing to read the Environment
-- is the behaviour we want.
readTextEnv :: Text -> IO Text
readTextEnv key = toS <$> getEnv (toS key)

readNumericEnv :: Text -> IO Int
readNumericEnv key = read . toS <$> getEnv (toS key)
