{-# OPTIONS -fno-warn-orphans #-}

-- We use this module to automatically generate types for consumption in the front-end
-- using the aeson-typescript module. This module is fantastic, with the one exception
-- that you have zero control over the output type name. What this means is that for
-- all our types that are named Request/Response we're going to generate clashes in
-- the output. This module produces our types and also offers a workaround for that
-- limitation.

module Commands.GenerateTypes
  ( commandGenerateTypes
  , generateTypes
  ) where

import Protolude
import Commands.Types             ( Options(..), GenerateTypesOptions(..), apiOptions )

import API.User.Types             ( User, DangerousUser )
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

-- Hack 1:
-- aeson-typescript doesn't export our types by default, new version of TS don't
-- like modules very miuch so we'll hand prefix our declarations here.

formatExports
  :: [TSDeclaration]
  -> Text

formatExports declarations =
  T.intercalate "\n\n"
    $   ("export " <>)
    .   pack
    .   formatTSDeclaration (FormattingOptions 4 (const "DIIICKS") (const "FUUUUCK"))
    <$> declarations

--------------------------------------------------------------------------------

-- Here we take all exposed API types, including all Route, Request/Response,
-- and HTTP level types, and generate a TypeSCript .d.ts file for use in the
-- front-end.
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

deriveTypeScript apiOptions ''AppendImageTags.Request
deriveTypeScript apiOptions ''AuthenticateUser.Request
deriveTypeScript apiOptions ''CreateImage.Request
deriveTypeScript apiOptions ''DangerousUser
deriveTypeScript apiOptions ''DeleteImageTags.Request
deriveTypeScript apiOptions ''FetchImageTags.Response
deriveTypeScript apiOptions ''FetchImages.Response
deriveTypeScript apiOptions ''RegisterUser.Request
deriveTypeScript apiOptions ''Token
deriveTypeScript apiOptions ''Types.APIImage
deriveTypeScript apiOptions ''User

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
