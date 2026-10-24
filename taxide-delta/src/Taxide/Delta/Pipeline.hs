{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Delta.Pipeline
Description : Orquestador integral para ingesta, validación y serialización de datasets DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Delta.Pipeline
  ( DeltaDataset (..)
  , readDeltaDataset
  , writeDeltaDataset
  , parseSpecsText
  , parseCharsText
  , parseItemsText
  , renderSpecs
  , renderChars
  , renderItems
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Taxide.Core.Character (CharacterDef (..))
import Taxide.Core.Item (ItemDef (..))
import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Delta.Parser.Chars (parseCharsText)
import Taxide.Delta.Parser.Items (parseItemsText)
import Taxide.Delta.Parser.Specs (parseSpecsText)
import Taxide.Delta.Serializer.Chars (renderChars)
import Taxide.Delta.Serializer.Items (renderItems)
import Taxide.Delta.Serializer.Specs (renderSpecs)

-- | Colección coherente de los tres archivos centrales de un dataset DELTA.
data DeltaDataset = DeltaDataset
  { dsSpecs :: !SpecsDef
  , dsChars :: ![CharacterDef]
  , dsItems :: ![ItemDef]
  } deriving (Eq, Show)

-- | Carga y analiza los archivos SPECS, CHARS e ITEMS desde un directorio dado.
readDeltaDataset :: FilePath -> IO (Either Text DeltaDataset)
readDeltaDataset dir = do
  let specsPath = dir </> "SPECS"
      charsPath = dir </> "CHARS"
      itemsPath = dir </> "ITEMS"

  sExists <- doesFileExist specsPath
  cExists <- doesFileExist charsPath
  iExists <- doesFileExist itemsPath

  if not (sExists && cExists && iExists)
    then pure $ Left $ "Error: El directorio '" <> T.pack dir <> "' debe contener los archivos SPECS, CHARS e ITEMS."
    else do
      specsContent <- TIO.readFile specsPath
      case parseSpecsText specsPath specsContent of
        Left errSpecs -> pure $ Left ("Error en SPECS: " <> errSpecs)
        Right specs -> do
          charsContent <- TIO.readFile charsPath
          case parseCharsText (specCharTypes specs) charsPath charsContent of
            Left errChars -> pure $ Left ("Error en CHARS: " <> errChars)
            Right chars -> do
              itemsContent <- TIO.readFile itemsPath
              case parseItemsText (specCharTypes specs) itemsPath itemsContent of
                Left errItems -> pure $ Left ("Error en ITEMS: " <> errItems)
                Right items -> pure $ Right $ DeltaDataset
                  { dsSpecs = specs
                  , dsChars = chars
                  , dsItems = items
                  }

-- | Escribe un DeltaDataset de forma canónica y limpia en un directorio dado.
writeDeltaDataset :: FilePath -> DeltaDataset -> IO ()
writeDeltaDataset outDir ds = do
  createDirectoryIfMissing True outDir
  TIO.writeFile (outDir </> "SPECS") (renderSpecs (dsSpecs ds))
  TIO.writeFile (outDir </> "CHARS") (renderChars (dsChars ds))
  TIO.writeFile (outDir </> "ITEMS") (renderItems (dsItems ds))
