module Configuration
  ( Config'(..)
  , Config
  , configConnection
  , configPort
  , configStaticLocation
  , readConfig
  , readNumericEnv
  , readTextEnv

  -- Monad Transformer
  , Pixel
  , MonadPixel
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens
import           HKD

import qualified Error                         as E
import qualified Eventless                     as Ev
import qualified Pixel                         as P
import qualified System.Environment            as S.E
import qualified Text.Read                     as T.R

--------------------------------------------------------------------------------

-- See `Imageless.hs` for an explanation of why we use `Make` functions to
-- generate Monads here. Otherwise, these are your standard Monad and MTL
-- constraints.
type Pixel = P.MakePixel E.PixelError Config
type MonadPixel m = P.MakeMonadPixel E.PixelError Config m

--------------------------------------------------------------------------------

-- HKD Configuration Definition
data Config' f = Config
  { _configStaticLocation :: HKD f Text
  , _configPort           :: HKD f Int
  , _configConnection     :: HKD f Ev.BackendStore
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

-- Environment Reading Helpers
--
-- These are intentionally partial, crashing on failing to read the Environment
-- is pretty much fine as the exception thrown is useful and it is before the
-- application has started doing anything meaningful.
readTextEnv :: Text -> IO Text
readTextEnv key = toS <$> S.E.getEnv (toS key)

readNumericEnv :: Text -> IO Int
readNumericEnv key = T.R.read . toS <$> S.E.getEnv (toS key)
