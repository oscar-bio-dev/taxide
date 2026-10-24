{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : CLI taxide-lint para auditoría semántica y validación de matrices DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Main (main) where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

import Taxide.Delta.Pipeline (DeltaDataset (..), readDeltaFiles)
import Taxide.Engine.Diagnostic
  ( LintDiagnostic (..)
  , Severity (..)
  , renderDiagnosticsReportJson
  , renderDiagnosticsReportTerminal
  )
import Taxide.Engine.Matrix (ColumnarMatrix (..))
import Taxide.Engine.Pipeline (PipelineResult (..), runPipeline)

-- | Opciones de invocación de taxide-lint en terminal.
data LintOptions = LintOptions
  { optInputDir :: !FilePath
  , optSpecs    :: !(Maybe FilePath)
  , optChars    :: !(Maybe FilePath)
  , optItems    :: !(Maybe FilePath)
  , optJson     :: !Bool
  , optVerbose  :: !Bool
  } deriving (Show, Eq)

-- | Parser de argumentos para optparse-applicative.
lintOptionsParser :: Parser LintOptions
lintOptionsParser = LintOptions
  <$> strOption
        ( long "input-dir"
       <> short 'i'
       <> metavar "DIR"
       <> value "."
       <> showDefault
       <> help "Directorio que contiene los archivos SPECS, CHARS e ITEMS" )
  <*> optional
        ( strOption
            ( long "specs"
           <> short 's'
           <> metavar "FILE"
           <> help "Ruta específica al archivo SPECS (omite detección en DIR)" ) )
  <*> optional
        ( strOption
            ( long "chars"
           <> short 'c'
           <> metavar "FILE"
           <> help "Ruta específica al archivo CHARS (omite detección en DIR)" ) )
  <*> optional
        ( strOption
            ( long "items"
           <> short 'm'
           <> metavar "FILE"
           <> help "Ruta específica al archivo ITEMS (omite detección en DIR)" ) )
  <*> switch
        ( long "json"
       <> help "Emite el informe de diagnósticos en formato JSON estructurado" )
  <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Muestra el desglose detallado de la matriz SoA resuelta" )

main :: IO ()
main = do
  opts <- execParser optsInfo
  let dir = optInputDir opts
      specsPath = maybe (dir </> "SPECS") id (optSpecs opts)
      charsPath = maybe (dir </> "CHARS") id (optChars opts)
      itemsPath = maybe (dir </> "ITEMS") id (optItems opts)

  -- Verificar existencia de archivos cardinales
  specsOk <- doesFileExist specsPath
  charsOk <- doesFileExist charsPath
  itemsOk <- doesFileExist itemsPath

  when (not specsOk || not charsOk || not itemsOk) $ do
    TIO.putStrLn $ "[ERROR] No se encontraron todos los archivos DELTA requeridos:"
    when (not specsOk) $ TIO.putStrLn $ "  - Falta SPECS en: " <> T.pack specsPath
    when (not charsOk) $ TIO.putStrLn $ "  - Falta CHARS en: " <> T.pack charsPath
    when (not itemsOk) $ TIO.putStrLn $ "  - Falta ITEMS en: " <> T.pack itemsPath
    exitFailure

  -- Ingesta sintáctica
  datasetRes <- readDeltaFiles specsPath charsPath itemsPath
  case datasetRes of
    Left parseErr -> do
      TIO.putStrLn $ "[ERROR SINTÁCTICO] Fallo al parsear los archivos DELTA:"
      TIO.putStrLn parseErr
      exitFailure

    Right dataset -> do
      -- Ejecución del pipeline semántico con cronometraje
      res <- runPipeline (dsSpecs dataset) (dsItems dataset)
      let diags = prDiagnostics res
          timeMs = prExecutionTimeMs res
          errCount = length [() | d <- diags, diagSeverity d == SeverityError]

      if optJson opts
        then TIO.putStrLn (renderDiagnosticsReportJson diags timeMs)
        else do
          TIO.putStrLn (renderDiagnosticsReportTerminal diags timeMs)
          when (optVerbose opts) $ do
            let mat = prMatrix res
            TIO.putStrLn "\n--- Métricas de Matriz SoA Resuelta ---"
            TIO.putStrLn $ "  * Taxones procesados: " <> T.pack (show (cmNumTaxa mat))
            TIO.putStrLn $ "  * Caracteres declarados: " <> T.pack (show (cmNumChars mat))
            TIO.putStrLn $ "  * Columnas discretas (Word64): " <> T.pack (show (length (cmDiscreteCols mat)))
            TIO.putStrLn $ "  * Columnas cuantitativas (SoA Double): " <> T.pack (show (length (cmNumericCols mat)))
            TIO.putStrLn $ "  * Columnas de texto descriptivo: " <> T.pack (show (length (cmTextCols mat)))

      if errCount > 0
        then exitFailure
        else exitSuccess
  where
    optsInfo = info (lintOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "Auditor semántico, validador del DAG e inferencia en memoria para TAXIDE"
     <> header "taxide-lint - Linter ontológico para matrices morfológicas" )
