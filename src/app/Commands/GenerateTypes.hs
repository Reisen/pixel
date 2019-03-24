{-# OPTIONS -fno-warn-orphans #-}

module Commands.GenerateTypes
  ( commandGenerateTypes
  , generateTypes
  ) where

--------------------------------------------------------------------------------

import           Protolude

import qualified API.Image.Routes              as API
import qualified API.Image.Types               as API
import           Commands.Types              ( Options (..), GenerateTypesOptions (..) )
import qualified Data.Aeson.TypeScript.TH      as TS
import qualified Options.Applicative           as O
import qualified Pixel                         as Pixel


--------------------------------------------------------------------------------

-- Here we take all exposed API types, including all
-- Route, Request/Response, and HTTP level types, and generate a TypeSCript
-- .d.ts file for use in the front-end.
--
-- This keeps our front-end and back-end in sync as much as possible.
generateTypes :: GenerateTypesOptions -> IO ()
generateTypes _ = do
  putText
    .  toS
    .  TS.formatTSDeclarations
    $  TS.getTypeScriptDeclarations (Proxy @API.Image)
    <> TS.getTypeScriptDeclarations (Proxy @API.DeleteTagsRequest)
    <> TS.getTypeScriptDeclarations (Proxy @API.GetImageResponse)
    <> TS.getTypeScriptDeclarations (Proxy @API.GetTagsResponse)
    <> TS.getTypeScriptDeclarations (Proxy @API.PostImageRequest)
    <> TS.getTypeScriptDeclarations (Proxy @API.PostTagsRequest)

  pure ()

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
  (  O.long "folder"
  <> O.help "Folder to store TypeScript definitions in."
  <> O.metavar "FOLDER"
  )
