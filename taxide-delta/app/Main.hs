{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : CLI taxide-fmt para formateo y validación sintáctica canónica de datasets DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Main (main) where

import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)

import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Delta.Pipeline (DeltaDataset (..), readDeltaDataset, writeDeltaDataset)

data FmtOptions = FmtOptions
  { optInputDir  :: !FilePath
  , optOutputDir :: !(Maybe FilePath)
  , optCheckOnly :: !Bool
  } deriving (Show)

fmtParser :: Parser FmtOptions
fmtParser = FmtOptions
  <$> strOption
      ( long "input-dir"
     <> short 'i'
     <> metavar "DIR"
     <> help "Directorio que contiene los archivos SPECS, CHARS e ITEMS"
     <> value "."
     <> showDefault
      )
  <*> optional (strOption
      ( long "output-dir"
     <> short 'o'
     <> metavar "OUTDIR"
     <> help "Directorio donde escribir los archivos DELTA reformateados canónicamente"
      ))
  <*> switch
      ( long "check"
     <> short 'c'
     <> help "Solo validar la sintaxis sin escribir archivos"
      )

optsInfo :: ParserInfo FmtOptions
optsInfo = info (fmtParser <**> helper)
  ( fullDesc
 <> progDesc "Formateador y validador sintáctico canónico de datasets DELTA para TAXIDE"
 <> header "taxide-fmt - Herramienta de normalización taxonómica"
  )

main :: IO ()
main = do
  opts <- execParser optsInfo
  let inDir = optInputDir opts

  putStrLn $ "Analizando dataset DELTA en: " ++ inDir
  res <- readDeltaDataset inDir
  case res of
    Left err -> do
      putStrLn "\n[ERROR DE SINTAXIS O ACCESO]:"
      TIO.putStrLn err
      exitFailure

    Right dataset -> do
      let specs = dsSpecs dataset
          chars = dsChars dataset
          items = dsItems dataset

      putStrLn "\n======================================================="
      putStrLn " [ÉXITO] Dataset DELTA sintácticamente válido"
      putStrLn "======================================================="
      putStrLn $ "  * Caracteres declarados en SPECS : " ++ show (specNumCharacters specs)
      putStrLn $ "  * Caracteres parseados en CHARS  : " ++ show (length chars)
      putStrLn $ "  * Taxones / Ítems en ITEMS       : " ++ show (length items)
      putStrLn "======================================================="

      case optOutputDir opts of
        Just outDir -> do
          putStrLn $ "Escribiendo dataset canónico en: " ++ outDir
          writeDeltaDataset outDir dataset
          putStrLn "Archivos SPECS, CHARS e ITEMS generados satisfactoriamente."
        Nothing ->
          if optCheckOnly opts
            then putStrLn "Modo --check: No se realizaron escrituras."
            else putStrLn "Sugerencia: Usa '-o <directorio>' para emitir el dataset formateado canónicamente."

      exitSuccess
