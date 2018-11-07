module Configuration
  ( Configuration (..)
  , readTextEnv
  , readNumericEnv

  -- Lens
  , HasStaticLocation (..)
  , HasPort           (..)
  , HasConnection     (..)

  -- Monad Transformer
  , Imageless
  , MonadImageless
  ) where


import Protolude
import Control.Lens
import Eventless              (BackendStore (..))
import Imageless              (MakeImageless, MakeMonadImageless)
import System.Environment     (getEnv)
import Text.Read              (read)


type Imageless = MakeImageless () Configuration
type MonadImageless m = MakeMonadImageless () Configuration m


data Configuration = Configuration
  { _configurationStaticLocation :: Text
  , _configurationPort           :: Int
  , _configurationConnection     :: BackendStore
  }

makeFields ''Configuration


readTextEnv :: Text -> IO Text
readTextEnv key = toS <$> getEnv (toS key)

readNumericEnv :: Text -> IO Int
readNumericEnv key = read . toS <$> getEnv (toS key)
