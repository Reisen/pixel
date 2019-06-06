{-# OPTIONS -fno-warn-orphans #-}

module Commands.GenerateTypes
  ( commandGenerateTypes
  , generateTypes
  ) where

import Protolude
import Commands.Types             ( Options (..), GenerateTypesOptions (..) )

import API.User.Types             ( User, DangerousUser )
import Data.Aeson                 ( defaultOptions )
import Data.Aeson.TypeScript.TH   ( TSDeclaration
                                  , FormattingOptions(..)
                                  , deriveTypeScript
                                  , formatTSDeclaration
                                  , getTypeScriptDeclarations
                                  )
import Data.Text as T             ( intercalate, pack )
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
import Pixel.Model.Token          ( Token )

import qualified Pixel.API.AppendImageTags  as AppendImageTags
import qualified Pixel.API.AuthenticateUser as AuthenticateUser
import qualified Pixel.API.CreateImage      as CreateImage
import qualified Pixel.API.DeleteImageTags  as DeleteImageTags
import qualified Pixel.API.FetchImageTags   as FetchImageTags
import qualified Pixel.API.FetchImages      as FetchImages
import qualified Pixel.API.RegisterUser     as RegisterUser
import qualified Pixel.API.Types            as Types

--------------------------------------------------------------------------------

formatExports
  :: [TSDeclaration]
  -> Text

formatExports declarations =
  T.intercalate "\n\n"
    $   ("export " <>)
    .   pack
    .   formatTSDeclaration (FormattingOptions 4)
    <$> declarations

-- Here we take all exposed API types, including all
-- Route, Request/Response, and HTTP level types, and generate a TypeSCript
-- .d.ts file for use in the front-end.
--
-- This keeps our front-end and back-end in sync as much as possible.
generateTypes :: GenerateTypesOptions -> IO ()
generateTypes (GenerateTypesOptions folder) = do
  putText $ "Creating " <> (show $ length definitions) <> " definitions."
  writeFile (toS folder) . formatExports $ definitions
  where
    definitions = fold
      [ getTypeScriptDeclarations (Proxy @Token)

      -- Image API Types
      , getTypeScriptDeclarations (Proxy @Types.APIImage)
      , getTypeScriptDeclarations (Proxy @DeleteImageTags.Request)
      , getTypeScriptDeclarations (Proxy @FetchImages.Response)
      , getTypeScriptDeclarations (Proxy @FetchImageTags.Response)
      , getTypeScriptDeclarations (Proxy @CreateImage.Request)
      , getTypeScriptDeclarations (Proxy @AppendImageTags.Request)

      -- User API Types
      , getTypeScriptDeclarations (Proxy @User)
      , getTypeScriptDeclarations (Proxy @AuthenticateUser.Request)
      , getTypeScriptDeclarations (Proxy @RegisterUser.Request)
      ]

--------------------------------------------------------------------------------
-- Generate TypeScript Instances

deriveTypeScript defaultOptions ''AppendImageTags.Request
deriveTypeScript defaultOptions ''AuthenticateUser.Request
deriveTypeScript defaultOptions ''CreateImage.Request
deriveTypeScript defaultOptions ''DangerousUser
deriveTypeScript defaultOptions ''DeleteImageTags.Request
deriveTypeScript defaultOptions ''FetchImageTags.Response
deriveTypeScript defaultOptions ''FetchImages.Response
deriveTypeScript defaultOptions ''RegisterUser.Request
deriveTypeScript defaultOptions ''Token
deriveTypeScript defaultOptions ''Types.APIImage
deriveTypeScript defaultOptions ''User

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
