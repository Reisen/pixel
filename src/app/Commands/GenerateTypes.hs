module Commands.GenerateTypes
  ( commandGenerateTypes
  , generateTypes
  ) where

import Protolude
import Commands.Types             ( Options(..), GenerateTypesOptions(..) )
import Data.Text as T             ( lines, isInfixOf, unlines, replace )
import Data.Text.IO               ( writeFile )
import Options.Applicative        ( Parser
                                  , Mod
                                  , CommandFields
                                  , command
                                  , help
                                  , info
                                  , long
                                  , metavar
                                  , progDesc
                                  , strOption
                                  )
import Pixel.API                  ( API )
import Servant.JS                 ( jsForAPI )
import Servant.JS.Axios           ( AxiosOptions(..), defAxiosOptions, axios )

--------------------------------------------------------------------------------

generateTypes :: GenerateTypesOptions -> IO ()
generateTypes (GenerateTypesOptions folder) = do
  -- Drop Header Lines (HACK: We don't really want headers ever)
  writeFile (toS folder) $ lines generatedAPI
    & filter (not . isInfixOf "headers: {")
    & map (T.replace "var " "export const ")
    & unlines
    & ("import { axios } from 'axios';\n" <>)

  where
    generatedAPI = jsForAPI (Proxy :: Proxy API) $ axios defAxiosOptions
      { withCredentials = True
      }

--------------------------------------------------------------------------------
-- Define primitives used for optparse

commandGenerateTypes :: Mod CommandFields Options
commandGenerateTypes = command "generate-types"
  (info parseOptions
    (progDesc "Generate TypeScript definitions for API."))

parseOptions :: Parser Options
parseOptions = GenerateTypes <$>
  (   GenerateTypesOptions
  <$> folderOption
  )

folderOption :: Parser Text
folderOption = strOption
  (  long "file"
  <> help "File to write TypeScript definitions into."
  <> metavar "FILE"
  )
