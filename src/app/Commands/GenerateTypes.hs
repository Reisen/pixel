{-# OPTIONS -fno-warn-orphans #-}

module Commands.GenerateTypes
  ( commandGenerateTypes
  , generateTypes
  ) where

--------------------------------------------------------------------------------

import           Protolude

import           Commands.Types                 ( Options (..), GenerateTypesOptions (..) )
import qualified API.Image.Routes              as API
import qualified API.Image.Types               as API
import qualified Data.Aeson.TypeScript.TH      as TS
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Options.Applicative           as O
import qualified Pixel                         as Pixel

--------------------------------------------------------------------------------

formatExports
  :: [TS.TSDeclaration]
  -> Text

formatExports declarations =
  T.intercalate "\n\n"
    $   ("export " <>)
    .   T.pack
    .   TS.formatTSDeclaration (TS.FormattingOptions 4)
    <$> declarations

-- Here we take all exposed API types, including all
-- Route, Request/Response, and HTTP level types, and generate a TypeSCript
-- .d.ts file for use in the front-end.
--
-- This keeps our front-end and back-end in sync as much as possible.
generateTypes :: GenerateTypesOptions -> IO ()
generateTypes (GenerateTypesOptions folder) = do
  putText $ "Creating " <> (show $ length definitions) <> " definitions."
  T.IO.writeFile (toS folder) . formatExports $ definitions
  where
    definitions =
      TS.getTypeScriptDeclarations (Proxy @API.Image)
        <> TS.getTypeScriptDeclarations (Proxy @API.DeleteTagsRequest)
        <> TS.getTypeScriptDeclarations (Proxy @API.GetImageResponse)
        <> TS.getTypeScriptDeclarations (Proxy @API.GetTagsResponse)
        <> TS.getTypeScriptDeclarations (Proxy @API.PostImageRequest)
        <> TS.getTypeScriptDeclarations (Proxy @API.PostTagsRequest)

--------------------------------------------------------------------------------
-- Generate TypeScript Instances

TS.deriveTypeScript
  (Pixel.createOptions @API.DeleteTagsRequest)
  ''API.DeleteTagsRequest

TS.deriveTypeScript
  (Pixel.createOptions @API.GetImageResponse)
  ''API.GetImageResponse

TS.deriveTypeScript
  (Pixel.createOptions @API.GetTagsResponse)
  ''API.GetTagsResponse

TS.deriveTypeScript
  (Pixel.createOptions @API.PostTagsRequest)
  ''API.PostTagsRequest

TS.deriveTypeScript
  (Pixel.createOptions @API.PostImageRequest)
  ''API.PostImageRequest

TS.deriveTypeScript
  (Pixel.createOptions @API.Image)
  ''API.Image

--------------------------------------------------------------------------------
-- Define primitives used for optparse

commandGenerateTypes :: O.Mod O.CommandFields Options
commandGenerateTypes = O.command "generate-types"
  (O.info parseOptions
    (O.progDesc "Generate TypeScript definitions for API."))

parseOptions :: O.Parser Options
parseOptions = GenerateTypes <$>
  (   GenerateTypesOptions
  <$> folderOption
  )

folderOption :: O.Parser Text
folderOption = O.strOption
  (  O.long "file"
  <> O.help "File to write TypeScript definitions into."
  <> O.metavar "FILE"
  )
