{-# OPTIONS -fno-warn-orphans #-}

module Commands.GenerateTypes
  ( commandGenerateTypes
  , generateTypes
  ) where

import Protolude
import Commands.Types             ( Options (..), GenerateTypesOptions (..) )

import API.User.Routes            ( AuthUserRequest, RegisterRequest )
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
import Pixel                      ( createOptions )
import Pixel.Model.Token          ( Token )

import qualified Pixel.API.AppendImageTags as AppendImageTags
import qualified Pixel.API.CreateImage     as CreateImage
import qualified Pixel.API.DeleteImageTags as DeleteImageTags
import qualified Pixel.API.FetchImages     as FetchImages
import qualified Pixel.API.FetchImageTags  as FetchImageTags
import qualified Pixel.API.Types           as Types

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
      , getTypeScriptDeclarations (Proxy @AuthUserRequest)
      , getTypeScriptDeclarations (Proxy @RegisterRequest)
      ]

--------------------------------------------------------------------------------
-- Generate TypeScript Instances

deriveTypeScript
  (createOptions @DeleteImageTags.Request)
  ''DeleteImageTags.Request

deriveTypeScript
  (createOptions @FetchImages.Response)
  ''FetchImages.Response

deriveTypeScript
  (createOptions @Types.APIImage)
  ''Types.APIImage

deriveTypeScript
  (createOptions @FetchImageTags.Response)
  ''FetchImageTags.Response

deriveTypeScript
  (createOptions @AppendImageTags.Request)
  ''AppendImageTags.Request

deriveTypeScript
  (createOptions @CreateImage.Request)
  ''CreateImage.Request

deriveTypeScript
  (createOptions @AuthUserRequest)
  ''AuthUserRequest

deriveTypeScript
  (createOptions @RegisterRequest)
  ''RegisterRequest

deriveTypeScript
  (createOptions @User)
  ''User

deriveTypeScript
  (createOptions @DangerousUser)
  ''DangerousUser

deriveTypeScript
  (createOptions @Token)
  ''Token

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
