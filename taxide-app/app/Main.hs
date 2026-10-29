{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Ejecutable maestro unificado 'taxide' (Release v1.0.0)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Orquesta mediante 'optparse-applicative' todos los subcomandos oficiales de TAXIDE:
  * @taxide tui@      : Interfaz interactiva de terminal (Brick / Vty Gruvbox 24-bit)
  * @taxide fmt@      : Formateador y validador sintáctico canónico DELTA
  * @taxide lint@     : Auditor semántico, ontológico y cladístico en memoria
  * @taxide compile@  : Compilador multi-objetivo (nexus, tnt, natural, dwca, json, hdf5)
  * @taxide describe@ : Síntesis directa de monografías en lenguaje natural (Markdown/HTML5)
  * @taxide tree@     : Inferencia cladística (Fitch/Sankoff, SPR/TBR) y fenética (Gower, UPGMA/NJ)
  * @taxide key@      : Generador de claves dicotómicas por ganancia de información de Shannon
-}

module Main (main) where

import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Numeric (showFFloat)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Core.Types (DatasetMetadata (..), emptyDatasetMetadata)
import Taxide.Delta.Pipeline (DeltaDataset (..), readDeltaFiles, writeDeltaDataset)
import Taxide.Engine.Diagnostic
  ( LintDiagnostic (..)
  , Severity (..)
  , renderDiagnosticsReportJson
  , renderDiagnosticsReportTerminal
  )
import Taxide.Engine.Discretize (DiscretizeMethod (..), DiscretizeTarget (..))
import Taxide.Engine.Gower (buildNeighborJoining, buildUPGMA, computeGowerMatrix)
import Taxide.Engine.Key
  ( KeyLang (..)
  , KeyOptions (..)
  , buildIdentificationKey
  , defaultKeyOptions
  , renderBracketedKey
  , renderIndentedKey
  , renderMarkdownKey
  )
import Taxide.Engine.Matrix (ColumnarMatrix (..))
import Taxide.Engine.Pipeline (PipelineResult (..), runPipeline)
import Taxide.Engine.Sankoff
  ( CharMetrics (..)
  , TreeEvaluation (..)
  , evaluateTreeParsimony
  )
import Taxide.Engine.Search
  ( SearchResult (..)
  , SearchStrategy (..)
  , runParsimonySearch
  )
import Taxide.Engine.Tree
  ( PhyloTree
  , mapTreeTaxa
  , renderAsciiTree
  , renderNewick
  )
import Taxide.Export.BinaryHPC (renderHpcBinary)
import Taxide.Export.Common (mkExportContext, renderExportError)
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
import Taxide.TUI.Runner (runTaxideTUIFiles)

-- | Subcomandos del ejecutable maestro 'taxide'.
data Command
  = CmdTUI      !TuiOpts
  | CmdFmt      !FmtOpts
  | CmdLint     !LintOpts
  | CmdCompile  !CompileOpts
  | CmdDescribe !DescribeOpts
  | CmdTree     !TreeOpts
  | CmdKey      !KeyOpts
  deriving (Eq, Show)

-- | Opciones comunes de localización de archivos DELTA.
data InputPaths = InputPaths
  { ipInputDir :: !FilePath
  , ipSpecs    :: !(Maybe FilePath)
  , ipChars    :: !(Maybe FilePath)
  , ipItems    :: !(Maybe FilePath)
  } deriving (Eq, Show)

data TuiOpts = TuiOpts
  { tuiDataset :: !FilePath
  , tuiSpecs   :: !(Maybe FilePath)
  , tuiChars   :: !(Maybe FilePath)
  , tuiItems   :: !(Maybe FilePath)
  } deriving (Eq, Show)

data FmtOpts = FmtOpts
  { fmtPaths     :: !InputPaths
  , fmtOutputDir :: !(Maybe FilePath)
  , fmtCheckOnly :: !Bool
  } deriving (Eq, Show)

data LintOpts = LintOpts
  { lintPaths   :: !InputPaths
  , lintJson    :: !Bool
  , lintVerbose :: !Bool
  } deriving (Eq, Show)

data Target = TNexus | TTnt | TNatural | TDwca | TJson | THdf5
  deriving (Eq, Show)

data NaturalFormat = FmtMarkdown | FmtHtml
  deriving (Eq, Show)

data CompileOpts = CompileOpts
  { compPaths      :: !InputPaths
  , compTarget     :: !Target
  , compOutput     :: !(Maybe FilePath)
  , compFormat     :: !NaturalFormat
  , compLang       :: !Lang
  , compDiscretize :: !DiscretizeMethod
  , compDiscTarget :: !DiscretizeTarget
  , compOmitImpl   :: !Bool
  , compNoUnits    :: !Bool
  , compNoText     :: !Bool
  , compNoHeadings :: !Bool
  , compTitle      :: !(Maybe Text)
  , compCreators   :: ![Text]
  , compAbstract   :: !(Maybe Text)
  , compLicense    :: !(Maybe Text)
  , compContact    :: !(Maybe Text)
  , compCitation   :: !(Maybe Text)
  , compIdentifier :: !(Maybe Text)
  , compForce      :: !Bool
  } deriving (Eq, Show)

data DescribeOpts = DescribeOpts
  { descPaths      :: !InputPaths
  , descOutput     :: !(Maybe FilePath)
  , descFormat     :: !NaturalFormat
  , descLang       :: !Lang
  , descOmitImpl   :: !Bool
  , descNoUnits    :: !Bool
  , descNoText     :: !Bool
  , descNoHeadings :: !Bool
  , descTitle      :: !(Maybe Text)
  , descForce      :: !Bool
  } deriving (Eq, Show)

data TreeMethod = MethodParsimony | MethodUPGMA | MethodNJ
  deriving (Eq, Show)

data TreeFormat = FmtTreeNewick | FmtTreeAscii | FmtTreeReport
  deriving (Eq, Show)

data TreeOpts = TreeOpts
  { treePaths      :: !InputPaths
  , treeMethod     :: !TreeMethod
  , treeSearch     :: !SearchStrategy
  , treeDiscretize :: !DiscretizeMethod
  , treeFormat     :: !TreeFormat
  , treeOutput     :: !(Maybe FilePath)
  , treeForce      :: !Bool
  } deriving (Eq, Show)

data KeyFormat = FmtKeyBracketed | FmtKeyIndented | FmtKeyMarkdown
  deriving (Eq, Show)

data KeyOpts = KeyOpts
  { keyPaths      :: !InputPaths
  , keyDiscretize :: !DiscretizeMethod
  , keyFormat     :: !KeyFormat
  , keyLang       :: !KeyLang
  , keyOutput     :: !(Maybe FilePath)
  , keyForce      :: !Bool
  } deriving (Eq, Show)

-- | Parsers auxiliares de optparse-applicative.
inputPathsParser :: Parser InputPaths
inputPathsParser = InputPaths
  <$> strOption
        ( long "input-dir" <> long "dataset" <> short 'i' <> metavar "DIR"
       <> value "." <> showDefault
       <> help "Directorio con los archivos SPECS, CHARS e ITEMS" )
  <*> optional (strOption (long "specs" <> short 's' <> metavar "FILE" <> help "Ruta específica a SPECS"))
  <*> optional (strOption (long "chars" <> short 'c' <> metavar "FILE" <> help "Ruta específica a CHARS"))
  <*> optional (strOption (long "items" <> short 'm' <> metavar "FILE" <> help "Ruta específica a ITEMS"))

tuiOptsParser :: Parser TuiOpts
tuiOptsParser = TuiOpts
  <$> strOption
        ( long "dataset" <> long "input-dir" <> short 'i' <> metavar "DIR"
       <> value "." <> showDefault
       <> help "Directorio del dataset DELTA para abrir en la TUI" )
  <*> optional (strOption (long "specs" <> short 's' <> metavar "FILE" <> help "Ruta específica a SPECS"))
  <*> optional (strOption (long "chars" <> short 'c' <> metavar "FILE" <> help "Ruta específica a CHARS"))
  <*> optional (strOption (long "items" <> short 'm' <> metavar "FILE" <> help "Ruta específica a ITEMS"))

fmtOptsParser :: Parser FmtOpts
fmtOptsParser = FmtOpts
  <$> inputPathsParser
  <*> optional
        ( strOption
            ( long "output-dir" <> short 'o' <> metavar "OUTDIR"
           <> help "Directorio donde escribir los archivos DELTA canónicos" ) )
  <*> switch
        ( long "check" <> help "Solo validar sintaxis sin escribir archivos" )

lintOptsParser :: Parser LintOpts
lintOptsParser = LintOpts
  <$> inputPathsParser
  <*> switch (long "json" <> help "Emite el informe de diagnósticos en formato JSON")
  <*> switch (long "verbose" <> short 'v' <> help "Muestra el desglose de la matriz SoA resuelta")

targetReader :: ReadM Target
targetReader = eitherReader $ \s -> case s of
  "nexus"   -> Right TNexus
  "tnt"     -> Right TTnt
  "natural" -> Right TNatural
  "dwca"    -> Right TDwca
  "json"    -> Right TJson
  "hdf5"    -> Right THdf5
  _ -> Left "objetivo inválido; use nexus|tnt|natural|dwca|json|hdf5"

natFormatReader :: ReadM NaturalFormat
natFormatReader = eitherReader $ \s -> case s of
  "md"       -> Right FmtMarkdown
  "markdown" -> Right FmtMarkdown
  "html"     -> Right FmtHtml
  _ -> Left "formato inválido; use md|html"

natLangReader :: ReadM Lang
natLangReader = eitherReader $ \s -> case s of
  "en" -> Right Taxide.Export.Natural.LangEn
  "es" -> Right Taxide.Export.Natural.LangEs
  _ -> Left "idioma inválido; use en|es"

discretizeReader :: ReadM DiscretizeMethod
discretizeReader = eitherReader $ \s -> case s of
  "sqrt"    -> Right SquareRootRule
  "sturges" -> Right SturgesRule
  _ -> case reads s of
         [(n, "")] | n >= 2 -> Right (FixedClasses n)
         _ -> Left "regla inválida; use sqrt|sturges|N (N >= 2)"

discTargetReader :: ReadM DiscretizeTarget
discTargetReader = eitherReader $ \s -> case s of
  "center"   -> Right UseCenterVal
  "midpoint" -> Right UseNormalMidpoint
  _ -> Left "valor inválido; use center|midpoint"

textOption :: Mod OptionFields Text -> Parser (Maybe Text)
textOption = optional . strOption

compileOptsParser :: Parser CompileOpts
compileOptsParser = CompileOpts
  <$> inputPathsParser
  <*> option targetReader (long "target" <> short 't' <> metavar "TARGET"
                           <> help "Formato de salida: nexus|tnt|natural|dwca|json|hdf5")
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE"
                           <> help "Archivo de salida (obligatorio para dwca y hdf5)"))
  <*> option natFormatReader (long "format" <> metavar "md|html" <> value FmtMarkdown
                              <> help "Formato del objetivo natural (por defecto md)")
  <*> option natLangReader (long "lang" <> metavar "en|es" <> value Taxide.Export.Natural.LangEn
                            <> help "Idioma de la prosa (por defecto en)")
  <*> option discretizeReader (long "discretize" <> metavar "sqrt|sturges|N" <> value SquareRootRule
                               <> help "Regla para discretizar IN/RN a OM (por defecto sqrt)")
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

describeOptsParser :: Parser DescribeOpts
describeOptsParser = DescribeOpts
  <$> inputPathsParser
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Archivo de salida"))
  <*> option natFormatReader (long "format" <> metavar "md|html" <> value FmtMarkdown <> help "Formato (md|html)")
  <*> option natLangReader (long "lang" <> metavar "en|es" <> value Taxide.Export.Natural.LangEs <> help "Idioma (en|es)")
  <*> switch (long "omit-implicit" <> help "Omite valores implícitos")
  <*> switch (long "no-units" <> help "No inserta unidades")
  <*> switch (long "no-text" <> help "No incluye caracteres TE")
  <*> switch (long "no-headings" <> help "No agrupa por subtítulos")
  <*> textOption (long "title" <> metavar "TEXT" <> help "Título de la monografía")
  <*> switch (long "force" <> help "Genera las descripciones aunque el linter reporte errores")

treeMethodReader :: ReadM TreeMethod
treeMethodReader = eitherReader $ \s -> case s of
  "parsimony" -> Right MethodParsimony
  "wagner"    -> Right MethodParsimony
  "spr"       -> Right MethodParsimony
  "tbr"       -> Right MethodParsimony
  "upgma"     -> Right MethodUPGMA
  "nj"        -> Right MethodNJ
  _           -> Left "método inválido; use parsimony|upgma|nj"

searchReader :: ReadM SearchStrategy
searchReader = eitherReader $ \s -> case s of
  "wagner" -> Right SearchWagner
  "spr"    -> Right SearchSPR
  "tbr"    -> Right SearchTBR
  _        -> Left "búsqueda inválida; use wagner|spr|tbr"

treeFormatReader :: ReadM TreeFormat
treeFormatReader = eitherReader $ \s -> case s of
  "newick" -> Right FmtTreeNewick
  "ascii"  -> Right FmtTreeAscii
  "both"   -> Right FmtTreeReport
  "report" -> Right FmtTreeReport
  _        -> Left "formato inválido; use newick|ascii|report"

treeOptsParser :: Parser TreeOpts
treeOptsParser = TreeOpts
  <$> inputPathsParser
  <*> option treeMethodReader
        ( long "method" <> metavar "parsimony|upgma|nj" <> value MethodParsimony <> showDefault
       <> help "Método analítico de reconstrucción" )
  <*> option searchReader
        ( long "search" <> metavar "wagner|spr|tbr" <> value SearchSPR <> showDefault
       <> help "Estrategia de búsqueda topológica para parsimonia" )
  <*> option discretizeReader
        ( long "discretize" <> metavar "sqrt|sturges|N" <> value SquareRootRule
       <> help "Regla de discretización para IN/RN" )
  <*> option treeFormatReader
        ( long "format" <> metavar "newick|ascii|report" <> value FmtTreeReport <> showDefault
       <> help "Formato de salida del árbol" )
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Archivo de salida"))
  <*> switch (long "force" <> help "Ejecuta aunque el linter reporte errores")

keyFormatReader :: ReadM KeyFormat
keyFormatReader = eitherReader $ \s -> case s of
  "bracketed" -> Right FmtKeyBracketed
  "indented"  -> Right FmtKeyIndented
  "md"        -> Right FmtKeyMarkdown
  "markdown"  -> Right FmtKeyMarkdown
  _           -> Left "formato inválido; use bracketed|indented|md"

keyLangReader :: ReadM KeyLang
keyLangReader = eitherReader $ \s -> case s of
  "en" -> Right Taxide.Engine.Key.LangEn
  "es" -> Right Taxide.Engine.Key.LangEs
  _    -> Left "idioma inválido; use en|es"

keyOptsParser :: Parser KeyOpts
keyOptsParser = KeyOpts
  <$> inputPathsParser
  <*> option discretizeReader
        ( long "discretize" <> metavar "sqrt|sturges|N" <> value SquareRootRule
       <> help "Regla de discretización cuantitativa para IN/RN" )
  <*> option keyFormatReader
        ( long "format" <> long "style" <> metavar "bracketed|indented|md"
       <> value FmtKeyBracketed <> showDefault
       <> help "Estilo de la clave dicotómica" )
  <*> option keyLangReader
        ( long "lang" <> metavar "en|es" <> value Taxide.Engine.Key.LangEn <> showDefault
       <> help "Idioma de encabezados y enlaces" )
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Archivo de salida"))
  <*> switch (long "force" <> help "Genera la clave aunque el linter reporte errores")

commandParser :: Parser Command
commandParser = hsubparser
  ( command "tui"
      (info (CmdTUI <$> tuiOptsParser)
        (progDesc "Inicia el entorno interactivo de terminal (TUI Gruvbox 24-bit TrueColor)"))
 <> command "fmt"
      (info (CmdFmt <$> fmtOptsParser)
        (progDesc "Formateador y validador sintáctico canónico de datasets DELTA"))
 <> command "lint"
      (info (CmdLint <$> lintOptsParser)
        (progDesc "Auditor semántico, validador del DAG e inferencia en memoria"))
 <> command "compile"
      (info (CmdCompile <$> compileOptsParser)
        (progDesc "Compilador multi-objetivo (nexus, tnt, natural, dwca, json, hdf5)"))
 <> command "describe"
      (info (CmdDescribe <$> describeOptsParser)
        (progDesc "Genera descripciones taxonómicas en lenguaje natural (Markdown/HTML5)"))
 <> command "tree"
      (info (CmdTree <$> treeOptsParser)
        (progDesc "Reconstrucción cladística (Fitch/Sankoff, SPR/TBR) y fenética (Gower, UPGMA/NJ)"))
 <> command "key"
      (info (CmdKey <$> keyOptsParser)
        (progDesc "Generador de claves dicotómicas óptimas mediante Entropía de Shannon"))
  )

-- | Resuelve las rutas de los tres archivos cardinales DELTA (permitiendo también que
-- si se pasa sólo @--items=ruta/ITEMS@, el directorio padre suministre @SPECS@ y @CHARS@).
resolveTriplePaths :: InputPaths -> (FilePath, FilePath, FilePath)
resolveTriplePaths ip =
  let baseDir = case (ipInputDir ip, ipItems ip) of
        (".", Just mPath) ->
          let d = takeDirectory mPath
          in if null d then "." else d
        (d, _) -> d
      s = maybe (baseDir </> "SPECS") id (ipSpecs ip)
      c = maybe (baseDir </> "CHARS") id (ipChars ip)
      m = maybe (baseDir </> "ITEMS") id (ipItems ip)
  in (s, c, m)

die' :: String -> IO a
die' msg = hPutStrLn stderr msg >> exitFailure

loadAndValidateDataset :: InputPaths -> IO DeltaDataset
loadAndValidateDataset ip = do
  let (s, c, m) = resolveTriplePaths ip
  oks <- mapM doesFileExist [s, c, m]
  unless (and oks) $
    die' ("[ERROR] Faltan archivos DELTA: " ++ unwords [p | (p, ok) <- zip [s, c, m] oks, not ok])
  res <- readDeltaFiles s c m
  case res of
    Left err -> die' ("[ERROR SINTÁCTICO] " ++ T.unpack err)
    Right ds -> pure ds

main :: IO ()
main = do
  cmd <- execParser optsInfo
  case cmd of
    CmdTUI o      -> runTuiCmd o
    CmdFmt o      -> runFmtCmd o
    CmdLint o     -> runLintCmd o
    CmdCompile o  -> runCompileCmd o
    CmdDescribe o -> runDescribeCmd o
    CmdTree o     -> runTreeCmd o
    CmdKey o      -> runKeyCmd o
  where
    optsInfo = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "TAXIDE v1.0.0 — Taxonomic Integrated Development Environment"
     <> header "taxide - Entorno Formal de Sistemática Morfológica y Cladística en Haskell" )

runTuiCmd :: TuiOpts -> IO ()
runTuiCmd o = do
  let dir = tuiDataset o
      s = maybe (dir </> "SPECS") id (tuiSpecs o)
      c = maybe (dir </> "CHARS") id (tuiChars o)
      m = maybe (dir </> "ITEMS") id (tuiItems o)
  runTaxideTUIFiles s c m

runFmtCmd :: FmtOpts -> IO ()
runFmtCmd o = do
  dataset <- loadAndValidateDataset (fmtPaths o)
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
  case fmtOutputDir o of
    Just outDir -> do
      putStrLn $ "Escribiendo dataset canónico en: " ++ outDir
      writeDeltaDataset outDir dataset
      putStrLn "Archivos SPECS, CHARS e ITEMS generados satisfactoriamente."
    Nothing ->
      if fmtCheckOnly o
        then putStrLn "Modo --check: No se realizaron escrituras."
        else putStrLn "Sugerencia: Usa '-o <directorio>' para emitir el dataset formateado canónicamente."
  exitSuccess

runLintCmd :: LintOpts -> IO ()
runLintCmd o = do
  dataset <- loadAndValidateDataset (lintPaths o)
  res <- runPipeline (dsSpecs dataset) (dsItems dataset)
  let diags = prDiagnostics res
      timeMs = prExecutionTimeMs res
      errCount = length [() | d <- diags, diagSeverity d == SeverityError]
  if lintJson o
    then TIO.putStrLn (renderDiagnosticsReportJson diags timeMs)
    else do
      TIO.putStrLn (renderDiagnosticsReportTerminal diags timeMs)
      when (lintVerbose o) $ do
        let mat = prMatrix res
        TIO.putStrLn "\n--- Métricas de Matriz SoA Resuelta ---"
        TIO.putStrLn $ "  * Taxones procesados: " <> T.pack (show (cmNumTaxa mat))
        TIO.putStrLn $ "  * Caracteres declarados: " <> T.pack (show (cmNumChars mat))
        TIO.putStrLn $ "  * Columnas discretas (Word64): " <> T.pack (show (length (cmDiscreteCols mat)))
        TIO.putStrLn $ "  * Columnas cuantitativas (SoA Double): " <> T.pack (show (length (cmNumericCols mat)))
        TIO.putStrLn $ "  * Columnas de texto descriptivo: " <> T.pack (show (length (cmTextCols mat)))
  if errCount > 0 then exitFailure else exitSuccess

data Output = OutText Text | OutBytes BL.ByteString

runCompileCmd :: CompileOpts -> IO ()
runCompileCmd o = do
  when (compTarget o `elem` [TDwca, THdf5] && compOutput o == Nothing) $
    die' "[ERROR] Los objetivos dwca y hdf5 son binarios: indique el archivo con --output."
  dataset <- loadAndValidateDataset (compPaths o)
  res <- runPipeline (dsSpecs dataset) (dsItems dataset)
  let diags  = prDiagnostics res
      errors = [d | d <- diags, diagSeverity d == SeverityError]
  unless (null errors) $ do
    TIO.hPutStrLn stderr (renderDiagnosticsReportTerminal diags (prExecutionTimeMs res))
    unless (compForce o) $
      die' "[ERROR] El linter reportó errores; corríjalos o use --force para compilar de todos modos."
  let defaultTitle =
        let n = takeFileName (ipInputDir (compPaths o))
        in if null n || n == "." then "TAXIDE dataset" else T.pack n
      baseMeta = emptyDatasetMetadata (maybe defaultTitle id (compTitle o))
      meta = baseMeta
        { datasetCreators   = compCreators o
        , datasetAbstract   = compAbstract o
        , datasetLicense    = maybe (datasetLicense baseMeta) Just (compLicense o)
        , datasetContact    = compContact o
        , datasetCitation   = compCitation o
        , datasetIdentifier = compIdentifier o
        }
      natural = defaultNaturalOptions
        { noLang          = compLang o
        , noOmitImplicit  = compOmitImpl o
        , noShowUnits     = not (compNoUnits o)
        , noIncludeText   = not (compNoText o)
        , noGroupHeadings = not (compNoHeadings o)
        }
      ctx = mkExportContext meta (dsSpecs dataset) (dsChars dataset) (prTaxa res) (prMatrix res)
      compiled = case compTarget o of
        TNexus   -> OutText <$> renderNexus (NexusOptions (compDiscretize o) (compDiscTarget o)) ctx
        TTnt     -> OutText <$> renderTnt (TntOptions (compDiscretize o) (compDiscTarget o)) ctx
        TNatural -> Right $ OutText $ case compFormat o of
                      FmtMarkdown -> renderNaturalMarkdown natural ctx
                      FmtHtml     -> renderNaturalHtml natural ctx
        TDwca    -> Right (OutBytes (renderDwcArchive (DwcOptions natural) ctx))
        TJson    -> Right (OutBytes (renderJson ctx))
        THdf5    -> Right (OutBytes (renderHpcBinary ctx))
  case compiled of
    Left err -> die' ("[ERROR DE EXPORTACIÓN] " ++ T.unpack (renderExportError err))
    Right out -> case (out, compOutput o) of
      (OutText t, Nothing)    -> TIO.putStr t
      (OutText t, Just path)  -> BL.writeFile path (BL.fromStrict (TE.encodeUtf8 t))
      (OutBytes b, Just path) -> BL.writeFile path b
      (OutBytes b, Nothing)   -> BL.putStr b

runDescribeCmd :: DescribeOpts -> IO ()
runDescribeCmd o =
  runCompileCmd CompileOpts
    { compPaths      = descPaths o
    , compTarget     = TNatural
    , compOutput     = descOutput o
    , compFormat     = descFormat o
    , compLang       = descLang o
    , compDiscretize = SquareRootRule
    , compDiscTarget = UseCenterVal
    , compOmitImpl   = descOmitImpl o
    , compNoUnits    = descNoUnits o
    , compNoText     = descNoText o
    , compNoHeadings = descNoHeadings o
    , compTitle      = descTitle o
    , compCreators   = []
    , compAbstract   = Nothing
    , compLicense    = Nothing
    , compContact    = Nothing
    , compCitation   = Nothing
    , compIdentifier = Nothing
    , compForce      = descForce o
    }

fmtDec :: Int -> Double -> Text
fmtDec prec d = T.pack (showFFloat (Just prec) d "")

renderTreeReport
  :: TreeMethod
  -> SearchStrategy
  -> PhyloTree Text
  -> TreeEvaluation
  -> Maybe (Int, Int)
  -> Text
renderTreeReport method strategy namedTree eval mSearchStats =
  let methodTitle = case method of
        MethodParsimony -> "Máxima Parsimonia (Fitch / Sankoff-Wagner) — Búsqueda " <> T.pack (show strategy)
        MethodUPGMA     -> "Fenética de Gower — Agrupamiento UPGMA"
        MethodNJ        -> "Fenética de Gower — Neighbor-Joining (NJ)"
      statsLines = case mSearchStats of
        Nothing -> []
        Just (iters, cands) ->
          [ "  * Iteraciones de mejora : " <> T.pack (show iters)
          , "  * Topologías evaluadas  : " <> T.pack (show cands)
          ]
      charRows =
        [ "  " <> padR 6 (T.pack (show (cmCharId cm)))
          <> padR 6 (T.pack (show (cmCharType cm)))
          <> padR 10 (fmtDec 2 (cmSteps cm))
          <> padR 10 (fmtDec 2 (cmMinSteps cm))
          <> padR 10 (fmtDec 2 (cmMaxSteps cm))
          <> padR 10 (fmtDec 4 (cmConsistency cm))
          <> fmtDec 4 (cmRetention cm)
        | cm <- teCharMetrics eval
        ]
  in T.unlines $
       [ "======================================================================"
       , "                  TAXIDE - Informe Filogenético                       "
       , "======================================================================"
       , "Método: " <> methodTitle
       , ""
       , "--- Topología (ASCII / Unicode) ---"
       , T.stripEnd (renderAsciiTree namedTree)
       , ""
       , "--- Representación Newick ---"
       , renderNewick namedTree
       , ""
       , "--- Índices Globales de Ajuste Cladístico ---"
       , "  * Longitud Total L(T)             : " <> fmtDec 2 (teTotalLength eval)
       , "  * Cambio Mínimo Teórico M         : " <> fmtDec 2 (teTotalMinSteps eval)
       , "  * Cambio Máximo en Estrella G     : " <> fmtDec 2 (teTotalMaxSteps eval)
       , "  * Índice de Consistencia (CI)     : " <> fmtDec 4 (teConsistencyIndex eval)
       , "  * Índice de Retención (RI)        : " <> fmtDec 4 (teRetentionIndex eval)
       , "  * Consistencia Reescalada (RC)    : " <> fmtDec 4 (teRescaledCI eval)
       , "  * Índice de Homoplasia (HI)       : " <> fmtDec 4 (teHomoplasyIndex eval)
       ] ++ statsLines ++
       [ ""
       , "--- Desglose por Carácter (s_k, m_k, g_k, ci_k, ri_k) ---"
       , "  " <> padR 6 "Char" <> padR 6 "Tipo" <> padR 10 "s_k"
            <> padR 10 "m_k" <> padR 10 "g_k" <> padR 10 "CI" <> "RI"
       , "  --------------------------------------------------------------------"
       ] ++ charRows ++
       [ "======================================================================" ]
  where
    padR n t = let len = T.length t in if len >= n then t else t <> T.replicate (n - len) " "

runTreeCmd :: TreeOpts -> IO ()
runTreeCmd o = do
  dataset <- loadAndValidateDataset (treePaths o)
  res <- runPipeline (dsSpecs dataset) (dsItems dataset)
  let diags  = prDiagnostics res
      errors = [d | d <- diags, diagSeverity d == SeverityError]
  unless (null errors) $ do
    TIO.hPutStrLn stderr (renderDiagnosticsReportTerminal diags (prExecutionTimeMs res))
    unless (treeForce o) $
      die' "[ERROR] El linter reportó errores; corríjalos o use --force para continuar."
  let specs = dsSpecs dataset
      taxa  = prTaxa res
      mat   = prMatrix res
      disc  = treeDiscretize o
      (idxTree, eval, mStats) = case treeMethod o of
        MethodParsimony ->
          let sr = runParsimonySearch (treeSearch o) disc UseCenterVal specs taxa mat
          in (srBestTree sr, srEvaluation sr, Just (srIterations sr, srCandidates sr))
        MethodUPGMA ->
          let dm = computeGowerMatrix specs mat
              t  = buildUPGMA dm
              ev = evaluateTreeParsimony disc UseCenterVal specs taxa mat t
          in (t, ev, Nothing)
        MethodNJ ->
          let dm = computeGowerMatrix specs mat
              t  = buildNeighborJoining dm
              ev = evaluateTreeParsimony disc UseCenterVal specs taxa mat t
          in (t, ev, Nothing)
      namedTree = mapTreeTaxa (cmTaxonNames mat) idxTree
      outText = case treeFormat o of
        FmtTreeNewick -> renderNewick namedTree <> "\n"
        FmtTreeAscii  -> renderAsciiTree namedTree
        FmtTreeReport -> renderTreeReport (treeMethod o) (treeSearch o) namedTree eval mStats
  case treeOutput o of
    Nothing   -> TIO.putStr outText
    Just path -> TIO.writeFile path outText

runKeyCmd :: KeyOpts -> IO ()
runKeyCmd o = do
  dataset <- loadAndValidateDataset (keyPaths o)
  res <- runPipeline (dsSpecs dataset) (dsItems dataset)
  let diags  = prDiagnostics res
      errors = [d | d <- diags, diagSeverity d == SeverityError]
  unless (null errors) $ do
    TIO.hPutStrLn stderr (renderDiagnosticsReportTerminal diags (prExecutionTimeMs res))
    unless (keyForce o) $
      die' "[ERROR] El linter reportó errores; corríjalos o use --force para continuar."
  let keyOpts = defaultKeyOptions
        { koDiscretizeMethod = keyDiscretize o
        , koDiscretizeTarget = UseCenterVal
        }
      keyTree = buildIdentificationKey keyOpts (dsSpecs dataset) (dsChars dataset) (prTaxa res) (prMatrix res)
      lang    = keyLang o
      outText = case keyFormat o of
        FmtKeyBracketed -> renderBracketedKey lang keyTree
        FmtKeyIndented  -> renderIndentedKey lang keyTree
        FmtKeyMarkdown  -> renderMarkdownKey lang keyTree
  case keyOutput o of
    Nothing   -> TIO.putStr outText
    Just path -> TIO.writeFile path outText

