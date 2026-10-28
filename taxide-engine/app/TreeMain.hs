{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : CLI taxide-tree: inferencia cladística (Fitch/Sankoff, SPR/TBR) y fenética (Gower, UPGMA/NJ)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Main (main) where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Numeric (showFFloat)
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
import Taxide.Engine.Gower
  ( buildNeighborJoining
  , buildUPGMA
  , computeGowerMatrix
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

-- | Método de reconstrucción filogenética o fenética.
data TreeMethod
  = MethodParsimony
  | MethodUPGMA
  | MethodNJ
  deriving (Eq, Show)

-- | Formato de salida del árbol.
data TreeFormat
  = FmtNewick
  | FmtAscii
  | FmtReport
  deriving (Eq, Show)

-- | Opciones de invocación de taxide-tree.
data TreeOptions = TreeOptions
  { optInputDir   :: !FilePath
  , optSpecs      :: !(Maybe FilePath)
  , optChars      :: !(Maybe FilePath)
  , optItems      :: !(Maybe FilePath)
  , optMethod     :: !TreeMethod
  , optSearch     :: !SearchStrategy
  , optDiscretize :: !DiscretizeMethod
  , optFormat     :: !TreeFormat
  , optOutput     :: !(Maybe FilePath)
  , optForce      :: !Bool
  } deriving (Eq, Show)

methodReader :: ReadM TreeMethod
methodReader = eitherReader $ \s -> case s of
  "parsimony" -> Right MethodParsimony
  "upgma"     -> Right MethodUPGMA
  "nj"        -> Right MethodNJ
  _           -> Left "método inválido; use parsimony|upgma|nj"

searchReader :: ReadM SearchStrategy
searchReader = eitherReader $ \s -> case s of
  "wagner" -> Right SearchWagner
  "spr"    -> Right SearchSPR
  "tbr"    -> Right SearchTBR
  _        -> Left "búsqueda inválida; use wagner|spr|tbr"

discretizeReader :: ReadM DiscretizeMethod
discretizeReader = eitherReader $ \s -> case s of
  "sqrt"    -> Right SquareRootRule
  "sturges" -> Right SturgesRule
  _ -> case reads s of
         [(n, "")] | n >= 2 -> Right (FixedClasses n)
         _ -> Left "regla inválida; use sqrt|sturges|N (N >= 2)"

formatReader :: ReadM TreeFormat
formatReader = eitherReader $ \s -> case s of
  "newick" -> Right FmtNewick
  "ascii"  -> Right FmtAscii
  "report" -> Right FmtReport
  _        -> Left "formato inválido; use newick|ascii|report"

treeOptionsParser :: Parser TreeOptions
treeOptionsParser = TreeOptions
  <$> strOption
        ( long "input-dir" <> short 'i' <> metavar "DIR" <> value "." <> showDefault
       <> help "Directorio con los archivos SPECS, CHARS e ITEMS" )
  <*> optional (strOption (long "specs" <> short 's' <> metavar "FILE" <> help "Ruta específica a SPECS"))
  <*> optional (strOption (long "chars" <> short 'c' <> metavar "FILE" <> help "Ruta específica a CHARS"))
  <*> optional (strOption (long "items" <> short 'm' <> metavar "FILE" <> help "Ruta específica a ITEMS"))
  <*> option methodReader
        ( long "method" <> metavar "parsimony|upgma|nj" <> value MethodParsimony <> showDefault
       <> help "Método analítico de reconstrucción (por defecto parsimony)" )
  <*> option searchReader
        ( long "search" <> metavar "wagner|spr|tbr" <> value SearchSPR <> showDefault
       <> help "Estrategia de búsqueda topológica para parsimonia (por defecto spr)" )
  <*> option discretizeReader
        ( long "discretize" <> metavar "sqrt|sturges|N" <> value SquareRootRule
       <> help "Regla de discretización cuantitativa para IN/RN (por defecto sqrt)" )
  <*> option formatReader
        ( long "format" <> metavar "newick|ascii|report" <> value FmtReport <> showDefault
       <> help "Formato de salida (por defecto report)" )
  <*> optional
        ( strOption
            ( long "output" <> short 'o' <> metavar "FILE"
           <> help "Archivo de salida (por defecto stdout)" ) )
  <*> switch
        ( long "force"
       <> help "Ejecuta la reconstrucción aunque el linter reporte errores" )

die' :: String -> IO a
die' msg = hPutStrLn stderr msg >> exitFailure

fmtDec :: Int -> Double -> Text
fmtDec prec d = T.pack (showFFloat (Just prec) d "")

renderReport
  :: TreeMethod
  -> SearchStrategy
  -> PhyloTree Text
  -> TreeEvaluation
  -> Maybe (Int, Int)
  -> Text
renderReport method strategy namedTree eval mSearchStats =
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
        [ "  " <> padRight 6 (T.pack (show (cmCharId cm)))
          <> padRight 6 (T.pack (show (cmCharType cm)))
          <> padRight 10 (fmtDec 2 (cmSteps cm))
          <> padRight 10 (fmtDec 2 (cmMinSteps cm))
          <> padRight 10 (fmtDec 2 (cmMaxSteps cm))
          <> padRight 10 (fmtDec 4 (cmConsistency cm))
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
       , "  " <> padRight 6 "Char" <> padRight 6 "Tipo" <> padRight 10 "s_k"
            <> padRight 10 "m_k" <> padRight 10 "g_k" <> padRight 10 "CI" <> "RI"
       , "  --------------------------------------------------------------------"
       ] ++ charRows ++
       [ "======================================================================" ]
  where
    padRight n txt =
      let len = T.length txt
      in if len >= n then txt else txt <> T.replicate (n - len) " "

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

  let specs = dsSpecs dataset
      taxa  = prTaxa res
      mat   = prMatrix res
      disc  = optDiscretize opts

      (idxTree, eval, mStats) = case optMethod opts of
        MethodParsimony ->
          let sr = runParsimonySearch (optSearch opts) disc UseCenterVal specs taxa mat
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
      outText = case optFormat opts of
        FmtNewick -> renderNewick namedTree <> "\n"
        FmtAscii  -> renderAsciiTree namedTree
        FmtReport -> renderReport (optMethod opts) (optSearch opts) namedTree eval mStats

  case optOutput opts of
    Nothing   -> TIO.putStr outText
    Just path -> TIO.writeFile path outText
  where
    optsInfo = info (treeOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "Reconstrucción cladística (Fitch/Sankoff, Wagner/SPR/TBR) y fenética (Gower, UPGMA/NJ)"
     <> header "taxide-tree - Motor Analítico Filogenético de TAXIDE" )

