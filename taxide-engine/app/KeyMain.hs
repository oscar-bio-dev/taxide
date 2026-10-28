{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : CLI taxide-key: generador de claves dicotómicas óptimas por Entropía de Shannon
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Taxide.Delta.Pipeline (DeltaDataset (..), readDeltaFiles)
import Taxide.Engine.Diagnostic
  ( LintDiagnostic (..)
  , Severity (..)
  , renderDiagnosticsReportTerminal
  )
import Taxide.Engine.Discretize (DiscretizeMethod (..), DiscretizeTarget (..))
import Taxide.Engine.Key
  ( KeyLang (..)
  , KeyOptions (..)
  , buildIdentificationKey
  , defaultKeyOptions
  , renderBracketedKey
  , renderIndentedKey
  , renderMarkdownKey
  )
import Taxide.Engine.Pipeline (PipelineResult (..), runPipeline)

-- | Formato de presentación de la clave dicotómica.
data KeyFormat
  = FmtBracketed
  | FmtIndented
  | FmtMarkdown
  deriving (Eq, Show)

-- | Opciones de invocación de taxide-key.
data KeyCliOptions = KeyCliOptions
  { optInputDir   :: !FilePath
  , optSpecs      :: !(Maybe FilePath)
  , optChars      :: !(Maybe FilePath)
  , optItems      :: !(Maybe FilePath)
  , optDiscretize :: !DiscretizeMethod
  , optFormat     :: !KeyFormat
  , optLang       :: !KeyLang
  , optOutput     :: !(Maybe FilePath)
  , optForce      :: !Bool
  } deriving (Eq, Show)

discretizeReader :: ReadM DiscretizeMethod
discretizeReader = eitherReader $ \s -> case s of
  "sqrt"    -> Right SquareRootRule
  "sturges" -> Right SturgesRule
  _ -> case reads s of
         [(n, "")] | n >= 2 -> Right (FixedClasses n)
         _ -> Left "regla inválida; use sqrt|sturges|N (N >= 2)"

formatReader :: ReadM KeyFormat
formatReader = eitherReader $ \s -> case s of
  "bracketed" -> Right FmtBracketed
  "indented"  -> Right FmtIndented
  "md"        -> Right FmtMarkdown
  "markdown"  -> Right FmtMarkdown
  _           -> Left "formato inválido; use bracketed|indented|md"

langReader :: ReadM KeyLang
langReader = eitherReader $ \s -> case s of
  "en" -> Right LangEn
  "es" -> Right LangEs
  _    -> Left "idioma inválido; use en|es"

keyOptionsParser :: Parser KeyCliOptions
keyOptionsParser = KeyCliOptions
  <$> strOption
        ( long "input-dir" <> short 'i' <> metavar "DIR" <> value "." <> showDefault
       <> help "Directorio con los archivos SPECS, CHARS e ITEMS" )
  <*> optional (strOption (long "specs" <> short 's' <> metavar "FILE" <> help "Ruta específica a SPECS"))
  <*> optional (strOption (long "chars" <> short 'c' <> metavar "FILE" <> help "Ruta específica a CHARS"))
  <*> optional (strOption (long "items" <> short 'm' <> metavar "FILE" <> help "Ruta específica a ITEMS"))
  <*> option discretizeReader
        ( long "discretize" <> metavar "sqrt|sturges|N" <> value SquareRootRule
       <> help "Regla de discretización cuantitativa para IN/RN (por defecto sqrt)" )
  <*> option formatReader
        ( long "format" <> metavar "bracketed|indented|md" <> value FmtBracketed <> showDefault
       <> help "Formato de salida de la clave (por defecto bracketed)" )
  <*> option langReader
        ( long "lang" <> metavar "en|es" <> value LangEn <> showDefault
       <> help "Idioma de encabezados y enlaces (por defecto en)" )
  <*> optional
        ( strOption
            ( long "output" <> short 'o' <> metavar "FILE"
           <> help "Archivo de salida (por defecto stdout)" ) )
  <*> switch
        ( long "force"
       <> help "Genera la clave aunque el linter reporte errores" )

die' :: String -> IO a
die' msg = hPutStrLn stderr msg >> exitFailure

main :: IO ()
main = do
  opts <- execParser optsInfo
  let dir = optInputDir opts
      specsPath = maybe (dir </> "SPECS") id (optSpecs opts)
      charsPath = maybe (dir </> "CHARS") id (optChars opts)
      itemsPath = maybe (dir </> "ITEMS") id (optItems opts)

  oks <- mapM doesFileExist [specsPath, charsPath, itemsPath]
  unless (and oks) $
    die' ("[ERROR] Faltan archivos DELTA: " ++ unwords [p | (p, ok) <- zip [specsPath, charsPath, itemsPath] oks, not ok])

  datasetRes <- readDeltaFiles specsPath charsPath itemsPath
  dataset <- case datasetRes of
    Left err -> die' ("[ERROR SINTÁCTICO] " ++ T.unpack err)
    Right d  -> pure d

  res <- runPipeline (dsSpecs dataset) (dsItems dataset)
  let diags  = prDiagnostics res
      errors = [d | d <- diags, diagSeverity d == SeverityError]
  unless (null errors) $ do
    TIO.hPutStrLn stderr (renderDiagnosticsReportTerminal diags (prExecutionTimeMs res))
    unless (optForce opts) $
      die' "[ERROR] El linter reportó errores; corríjalos o use --force para continuar."

  let keyOpts = defaultKeyOptions
        { koDiscretizeMethod = optDiscretize opts
        , koDiscretizeTarget = UseCenterVal
        }
      keyTree = buildIdentificationKey
                  keyOpts
                  (dsSpecs dataset)
                  (dsChars dataset)
                  (prTaxa res)
                  (prMatrix res)
      lang = optLang opts
      outText = case optFormat opts of
        FmtBracketed -> renderBracketedKey lang keyTree
        FmtIndented  -> renderIndentedKey lang keyTree
        FmtMarkdown  -> renderMarkdownKey lang keyTree

  case optOutput opts of
    Nothing   -> TIO.putStr outText
    Just path -> TIO.writeFile path outText
  where
    optsInfo = info (keyOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "Generador de claves dicotómicas óptimas mediante ganancia de información de Shannon"
     <> header "taxide-key - Generador de Claves de Identificación de TAXIDE" )

