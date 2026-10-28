{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : CLI taxide-compile: compilador multi-objetivo de matrices DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Main (main) where

import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

import Taxide.Core.Types (DatasetMetadata (..), emptyDatasetMetadata)
import Taxide.Delta.Pipeline (DeltaDataset (..), readDeltaFiles)
import Taxide.Engine.Diagnostic (LintDiagnostic (..), Severity (..), renderDiagnosticsReportTerminal)
import Taxide.Engine.Discretize (DiscretizeMethod (..), DiscretizeTarget (..))
import Taxide.Engine.Pipeline (PipelineResult (..), runPipeline)
import Taxide.Export.BinaryHPC (renderHpcBinary)
import Taxide.Export.Common (ExportContext, ExportError, mkExportContext, renderExportError)
import Taxide.Export.DarwinCore (DwcOptions (..), renderDwcArchive)
import Taxide.Export.Json (renderJson)
import Taxide.Export.Natural
  ( Lang (..)
  , NaturalOptions (..)
  , defaultNaturalOptions
  , renderNaturalHtml
  , renderNaturalMarkdown
  )
import Taxide.Export.Nexus (NexusOptions (..), renderNexus)
import Taxide.Export.TNT (TntOptions (..), renderTnt)

-- | Formato de salida.
data Target = TNexus | TTnt | TNatural | TDwca | TJson | THdf5
  deriving (Eq, Show)

-- | Formato de la salida en lenguaje natural.
data NaturalFormat = FmtMarkdown | FmtHtml
  deriving (Eq, Show)

-- | Opciones de invocación.
data Options = Options
  { optInputDir   :: !FilePath
  , optSpecs      :: !(Maybe FilePath)
  , optChars      :: !(Maybe FilePath)
  , optItems      :: !(Maybe FilePath)
  , optTarget     :: !Target
  , optOutput     :: !(Maybe FilePath)
  , optFormat     :: !NaturalFormat
  , optLang       :: !Lang
  , optDiscretize :: !DiscretizeMethod
  , optDiscTarget :: !DiscretizeTarget
  , optOmitImpl   :: !Bool
  , optNoUnits    :: !Bool
  , optNoText     :: !Bool
  , optNoHeadings :: !Bool
  , optTitle      :: !(Maybe Text)
  , optCreators   :: ![Text]
  , optAbstract   :: !(Maybe Text)
  , optLicense    :: !(Maybe Text)
  , optContact    :: !(Maybe Text)
  , optCitation   :: !(Maybe Text)
  , optIdentifier :: !(Maybe Text)
  , optForce      :: !Bool
  } deriving (Show, Eq)

targetReader :: ReadM Target
targetReader = eitherReader $ \s -> case s of
  "nexus"   -> Right TNexus
  "tnt"     -> Right TTnt
  "natural" -> Right TNatural
  "dwca"    -> Right TDwca
  "json"    -> Right TJson
  "hdf5"    -> Right THdf5
  _ -> Left "objetivo inválido; use nexus|tnt|natural|dwca|json|hdf5"

formatReader :: ReadM NaturalFormat
formatReader = eitherReader $ \s -> case s of
  "md"       -> Right FmtMarkdown
  "markdown" -> Right FmtMarkdown
  "html"     -> Right FmtHtml
  _ -> Left "formato inválido; use md|html"

langReader :: ReadM Lang
langReader = eitherReader $ \s -> case s of
  "en" -> Right LangEn
  "es" -> Right LangEs
  _ -> Left "idioma inválido; use en|es"

discretizeReader :: ReadM DiscretizeMethod
discretizeReader = eitherReader $ \s -> case s of
  "sqrt"    -> Right SquareRootRule
  "sturges" -> Right SturgesRule
  _ -> case reads s of
         [(n, "")] | n >= 2 -> Right (FixedClasses n)
         _ -> Left "método inválido; use sqrt|sturges|N (N >= 2 clases fijas)"

discTargetReader :: ReadM DiscretizeTarget
discTargetReader = eitherReader $ \s -> case s of
  "center"   -> Right UseCenterVal
  "midpoint" -> Right UseNormalMidpoint
  _ -> Left "valor inválido; use center|midpoint"

textOption :: Mod OptionFields Text -> Parser (Maybe Text)
textOption = optional . strOption

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (long "input-dir" <> short 'i' <> metavar "DIR" <> value "." <> showDefault
                 <> help "Directorio con los archivos SPECS, CHARS e ITEMS")
  <*> optional (strOption (long "specs" <> short 's' <> metavar "FILE" <> help "Ruta específica a SPECS"))
  <*> optional (strOption (long "chars" <> short 'c' <> metavar "FILE" <> help "Ruta específica a CHARS"))
  <*> optional (strOption (long "items" <> short 'm' <> metavar "FILE" <> help "Ruta específica a ITEMS"))
  <*> option targetReader (long "target" <> short 't' <> metavar "TARGET"
                           <> help "Formato de salida: nexus|tnt|natural|dwca|json|hdf5")
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE"
                           <> help "Archivo de salida (obligatorio para dwca y hdf5; por defecto stdout)"))
  <*> option formatReader (long "format" <> metavar "md|html" <> value FmtMarkdown
                           <> help "Formato del objetivo natural (por defecto md)")
  <*> option langReader (long "lang" <> metavar "en|es" <> value LangEn <> help "Idioma de la prosa (por defecto en)")
  <*> option discretizeReader (long "discretize" <> metavar "sqrt|sturges|N" <> value SquareRootRule
                               <> help "Regla para discretizar IN/RN a OM en nexus/tnt (por defecto sqrt)")
  <*> option discTargetReader (long "discretize-value" <> metavar "center|midpoint" <> value UseCenterVal
                               <> help "Valor representativo del rango (por defecto center)")
  <*> switch (long "omit-implicit" <> help "Omite en la prosa los valores provenientes de *IMPLICIT VALUES")
  <*> switch (long "no-units" <> help "No inserta unidades en caracteres IN/RN de la prosa")
  <*> switch (long "no-text" <> help "No incluye caracteres TE en la prosa")
  <*> switch (long "no-headings" <> help "No agrupa la prosa por subtítulos temáticos")
  <*> textOption (long "title" <> metavar "TEXT" <> help "Título del dataset (EML, prosa)")
  <*> many (strOption (long "creator" <> metavar "TEXT" <> help "Autor del dataset (repetible)"))
  <*> textOption (long "abstract" <> metavar "TEXT" <> help "Resumen del dataset")
  <*> textOption (long "license" <> metavar "TEXT" <> help "Licencia (por defecto CC-BY-4.0)")
  <*> textOption (long "contact" <> metavar "TEXT" <> help "Contacto o correo")
  <*> textOption (long "citation" <> metavar "TEXT" <> help "Cita sugerida")
  <*> textOption (long "identifier" <> metavar "TEXT" <> help "Identificador persistente o DOI")
  <*> switch (long "force" <> help "Compila aunque el linter reporte errores")

-- | Metadatos del dataset a partir de las opciones.
metadataOf :: Options -> DatasetMetadata
metadataOf o =
  let base = emptyDatasetMetadata (maybe defaultTitle id (optTitle o))
      defaultTitle = let n = takeFileName (optInputDir o)
                     in if null n || n == "." then "TAXIDE dataset" else T.pack n
  in base
       { datasetCreators   = optCreators o
       , datasetAbstract   = optAbstract o
       , datasetLicense    = maybe (datasetLicense base) Just (optLicense o)
       , datasetContact    = optContact o
       , datasetCitation   = optCitation o
       , datasetIdentifier = optIdentifier o
       }

-- | Salida del compilador: texto o binario.
data Output = OutText Text | OutBytes BL.ByteString

compile :: Options -> ExportContext -> Either ExportError Output
compile o ctx = case optTarget o of
  TNexus   -> OutText <$> renderNexus (NexusOptions (optDiscretize o) (optDiscTarget o)) ctx
  TTnt     -> OutText <$> renderTnt (TntOptions (optDiscretize o) (optDiscTarget o)) ctx
  TNatural -> Right $ OutText $ case optFormat o of
                FmtMarkdown -> renderNaturalMarkdown natural ctx
                FmtHtml     -> renderNaturalHtml natural ctx
  TDwca    -> Right (OutBytes (renderDwcArchive (DwcOptions natural) ctx))
  TJson    -> Right (OutBytes (renderJson ctx))
  THdf5    -> Right (OutBytes (renderHpcBinary ctx))
  where
    natural = defaultNaturalOptions
      { noLang          = optLang o
      , noOmitImplicit  = optOmitImpl o
      , noShowUnits     = not (optNoUnits o)
      , noIncludeText   = not (optNoText o)
      , noGroupHeadings = not (optNoHeadings o)
      }

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

  when (optTarget opts `elem` [TDwca, THdf5] && optOutput opts == Nothing) $
    die' "[ERROR] Los objetivos dwca y hdf5 son binarios: indique el archivo con --output."

  datasetRes <- readDeltaFiles specsPath charsPath itemsPath
  dataset <- case datasetRes of
    Left err -> die' ("[ERROR SINTÁCTICO] " ++ T.unpack err)
    Right d  -> pure d

  res <- runPipeline (dsSpecs dataset) (dsItems dataset)
  let diags = prDiagnostics res
      errors = [d | d <- diags, diagSeverity d == SeverityError]
  unless (null errors) $ do
    TIO.hPutStrLn stderr (renderDiagnosticsReportTerminal diags (prExecutionTimeMs res))
    unless (optForce opts) $
      die' "[ERROR] El linter reportó errores; corríjalos o use --force para compilar de todos modos."

  let ctx = mkExportContext (metadataOf opts) (dsSpecs dataset) (dsChars dataset) (prTaxa res) (prMatrix res)
  case compile opts ctx of
    Left err -> die' ("[ERROR DE EXPORTACIÓN] " ++ T.unpack (renderExportError err))
    Right out -> case (out, optOutput opts) of
      (OutText t, Nothing)    -> TIO.putStr t
      (OutText t, Just path)  -> BL.writeFile path (BL.fromStrict (TE.encodeUtf8 t))
      (OutBytes b, Just path) -> BL.writeFile path b
      (OutBytes b, Nothing)   -> BL.putStr b
  where
    optsInfo = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Compilador multi-objetivo: NEXUS, TNT, prosa natural, Darwin Core Archive, JSON y binario HPC"
     <> header "taxide-compile - Fase 3 de TAXIDE" )
