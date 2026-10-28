{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Engine.Diagnostic
Description : Sistema de diagnósticos estructurados, severidades y linters biológicos
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Engine.Diagnostic
  ( Severity (..)
  , DiagnosticLocation (..)
  , emptyLocation
  , taxonLoc
  , charLoc
  , taxonCharLoc
  , LintDiagnostic (..)
  , EngineError (..)
  , engineErrorToDiagnostic
  , invariantCharDiag
  , autapomorphyDiag
  , uncodedCharDiag
  , duplicateTaxonDiag
  , incoherentRangeDiag
  , indistinguishableTaxaDiag
  , renderDiagnosticTerminal
  , renderDiagnosticJson
  , renderDiagnosticsReportTerminal
  , renderDiagnosticsReportJson
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Numeric (showFFloat)

import Taxide.Core.Types (CharacterType (..))

-- | Nivel de severidad para diagnósticos e inspecciones del linter.
data Severity
  = SeverityError    -- ^ Error crítico que invalida la inferencia o viola la ontología biológica
  | SeverityWarning  -- ^ Advertencia sobre inconsistencia morfológica o sospecha de error
  | SeverityInfo     -- ^ Información contextual o sugerencia de optimización de matriz
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Ubicación precisa del diagnóstico en el conjunto de datos taxonómicos.
data DiagnosticLocation = DiagnosticLocation
  { locFile   :: !(Maybe Text)  -- ^ Archivo fuente (ej. "ITEMS", "SPECS")
  , locTaxon  :: !(Maybe Text)  -- ^ Nombre del taxón o ítem infractor
  , locCharId :: !(Maybe Int)   -- ^ Identificador numérico del carácter
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Ubicación vacía por defecto.
emptyLocation :: DiagnosticLocation
emptyLocation = DiagnosticLocation Nothing Nothing Nothing

-- | Ubicación asociada a un taxón.
taxonLoc :: Text -> DiagnosticLocation
taxonLoc name = DiagnosticLocation Nothing (Just name) Nothing

-- | Ubicación asociada a un carácter.
charLoc :: Int -> DiagnosticLocation
charLoc cid = DiagnosticLocation Nothing Nothing (Just cid)

-- | Ubicación asociada a un taxón y un carácter simultáneamente.
taxonCharLoc :: Text -> Int -> DiagnosticLocation
taxonCharLoc name cid = DiagnosticLocation Nothing (Just name) (Just cid)

-- | Diagnóstico estructurado emitido por el compilador o linter de TAXIDE.
data LintDiagnostic = LintDiagnostic
  { diagCode     :: !Text                -- ^ Código de regla taxonómica (ej. "TAX-DEP-001")
  , diagSeverity :: !Severity            -- ^ Nivel de severidad
  , diagLocation :: !DiagnosticLocation  -- ^ Coordenadas en el dataset
  , diagMessage  :: !Text                -- ^ Explicación detallada del problema biológico
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Errores formales del motor de inferencia semántica.
data EngineError
  = OrphanVariantError !Text
  -- ^ Ítem variante (#+) huérfano sin taxón principal (#) precedente
  | InvalidImplicitTargetType !Int !CharacterType
  -- ^ *IMPLICIT VALUES intentando aplicarse a un carácter no cualitativo
  | CyclicDependencyError ![Int]
  -- ^ Ciclo dirigido detectado en el grafo de *DEPENDENT CHARACTERS
  | ContradictoryDependentStateError !Text !Int !Int
  -- ^ Estado explícito dependiente contradice la inhabilitación del controlador (taxon, ctrl, dep)
  | DuplicateCharacterError !Text !Int !Int
  -- ^ Carácter codificado más de una vez en el mismo ítem (taxon, carácter, nº de apariciones)
  deriving (Eq, Show, Generic, NFData)

-- | Transforma un error formal de inferencia en un diagnóstico estructurado.
engineErrorToDiagnostic :: EngineError -> LintDiagnostic
engineErrorToDiagnostic err = case err of
  OrphanVariantError taxonName ->
    LintDiagnostic
      { diagCode     = "TAX-VAR-001"
      , diagSeverity = SeverityError
      , diagLocation = taxonLoc taxonName
      , diagMessage  = "El ítem variante '#+ " <> taxonName <> "' es huérfano: no existe un taxón principal '#' previo del cual heredar atributos."
      }

  InvalidImplicitTargetType cid ctype ->
    LintDiagnostic
      { diagCode     = "TAX-IMP-001"
      , diagSeverity = SeverityError
      , diagLocation = charLoc cid
      , diagMessage  = "Directiva *IMPLICIT VALUES inválida: el carácter " <> T.pack (show cid) <> " es de tipo " <> T.pack (show ctype) <> "; solo se admiten caracteres cualitativos (UM, OM, EUM, EOM)."
      }

  CyclicDependencyError cycleNodes ->
    LintDiagnostic
      { diagCode     = "TAX-DEP-001"
      , diagSeverity = SeverityError
      , diagLocation = emptyLocation
      , diagMessage  = "Dependencia circular detectada en *DEPENDENT CHARACTERS: ciclo [" <> T.intercalate " -> " (map (T.pack . show) cycleNodes) <> "]."
      }

  ContradictoryDependentStateError taxonName ctrlId depId ->
    LintDiagnostic
      { diagCode     = "TAX-DEP-002"
      , diagSeverity = SeverityError
      , diagLocation = taxonCharLoc taxonName depId
      , diagMessage  = "Violación biológica: en el taxón '" <> taxonName <> "', el carácter dependiente " <> T.pack (show depId)
                    <> " tiene un estado activo explícito a pesar de que su controlador " <> T.pack (show ctrlId)
                    <> " se encuentra en estado inhabilitante. El estado ha sido forzado a Inapplicable ('-')."
      }

  DuplicateCharacterError taxonName cid count ->
    LintDiagnostic
      { diagCode     = "TAX-DATA-003"
      , diagSeverity = SeverityError
      , diagLocation = taxonCharLoc taxonName cid
      , diagMessage  = "El carácter " <> T.pack (show cid) <> " está codificado " <> T.pack (show count)
                    <> " veces en el taxón '" <> taxonName <> "'; solo se conserva el último valor y los anteriores se descartan."
      }

-- | TAX-CLAD-001: Carácter invariante sin variación informativa.
invariantCharDiag :: Int -> Int -> LintDiagnostic
invariantCharDiag cid nTaxa =
  LintDiagnostic
    { diagCode     = "TAX-CLAD-001"
    , diagSeverity = SeverityWarning
    , diagLocation = charLoc cid
    , diagMessage  = "Carácter invariante: el carácter " <> T.pack (show cid)
                  <> " presenta exactamente el mismo estado en todos los taxones codificados ("
                  <> T.pack (show nTaxa) <> " taxones); no aporta información topológica para máxima parsimonia."
    }

-- | TAX-CLAD-002: Autapomorfía no informativa para filogenia.
autapomorphyDiag :: Int -> Text -> Int -> LintDiagnostic
autapomorphyDiag cid autTaxon countCommon =
  LintDiagnostic
    { diagCode     = "TAX-CLAD-002"
    , diagSeverity = SeverityInfo
    , diagLocation = taxonCharLoc autTaxon cid
    , diagMessage  = "Autapomorfía no informativa para parsimonia: en el carácter " <> T.pack (show cid)
                  <> ", el taxón '" <> autTaxon <> "' posee un estado derivado único frente a "
                  <> T.pack (show countCommon) <> " taxones con estado basal; útil para diagnóstico pero no resuelve clados."
    }

-- | TAX-DATA-001: Carácter no codificado en ningún taxón.
uncodedCharDiag :: Int -> Int -> LintDiagnostic
uncodedCharDiag cid nTaxa =
  LintDiagnostic
    { diagCode     = "TAX-DATA-001"
    , diagSeverity = SeverityWarning
    , diagLocation = charLoc cid
    , diagMessage  = "Carácter no codificado: el carácter " <> T.pack (show cid)
                  <> " carece de datos válidos (100% Unknown o Inapplicable) en todos los taxones evaluados ("
                  <> T.pack (show nTaxa) <> " taxones). Se sugiere su exclusión o eliminación del dataset."
    }

-- | TAX-DATA-002: Declaración duplicada de taxón primario.
duplicateTaxonDiag :: Text -> Maybe Text -> Int -> LintDiagnostic
duplicateTaxonDiag name mAuth count =
  LintDiagnostic
    { diagCode     = "TAX-DATA-002"
    , diagSeverity = SeverityWarning
    , diagLocation = taxonLoc name
    , diagMessage  = "Taxón duplicado: se detectaron " <> T.pack (show count)
                  <> " declaraciones del taxón principal '# " <> name
                  <> maybe "" (\a -> " <" <> a <> ">") mAuth
                  <> "'; los nombres de taxones primarios deben ser unívocos."
    }

-- | TAX-MATH-001: Incoherencia matemática en rangos estadísticos cuantitativos.
incoherentRangeDiag :: Text -> Int -> LintDiagnostic
incoherentRangeDiag taxonName cid =
  LintDiagnostic
    { diagCode     = "TAX-MATH-001"
    , diagSeverity = SeverityError
    , diagLocation = taxonCharLoc taxonName cid
    , diagMessage  = "Incoherencia matemática en rango cuantitativo del carácter " <> T.pack (show cid)
                  <> ": los valores de StatRange violan la progresión extremeMin <= normalLow <= centralTend <= normalHigh <= extremeMax."
    }

-- | TAX-WARN-001: Taxones biológicamente indistinguibles.
indistinguishableTaxaDiag :: Text -> Text -> LintDiagnostic
indistinguishableTaxaDiag nameA nameB =
  LintDiagnostic
    { diagCode     = "TAX-WARN-001"
    , diagSeverity = SeverityWarning
    , diagLocation = DiagnosticLocation Nothing (Just (nameA <> " / " <> nameB)) Nothing
    , diagMessage  = "Taxones biológicamente indistinguibles: '" <> nameA <> "' y '" <> nameB
                  <> "' poseen vectores de atributos morfológicos idénticos (excluyendo caracteres de texto libre TE); "
                  <> "generarán politomías irresolubles (zero-length branches) en análisis filogenéticos."
    }

-- | Renderiza un diagnóstico individual en formato legible de terminal.
renderDiagnosticTerminal :: LintDiagnostic -> Text
renderDiagnosticTerminal d =
  let sevStr = case diagSeverity d of
        SeverityError   -> "[ERROR]  "
        SeverityWarning -> "[AVISO]  "
        SeverityInfo    -> "[INFO]   "
      locStr = case (locTaxon (diagLocation d), locCharId (diagLocation d)) of
        (Just t, Just c)  -> " [Taxón: " <> t <> ", Carácter: " <> T.pack (show c) <> "]"
        (Just t, Nothing) -> " [Taxón: " <> t <> "]"
        (Nothing, Just c) -> " [Carácter: " <> T.pack (show c) <> "]"
        (Nothing, Nothing)-> ""
  in sevStr <> "[" <> diagCode d <> "]" <> locStr <> ": " <> diagMessage d

-- | Escapa cadenas para formato JSON estándar.
escapeJson :: Text -> Text
escapeJson = T.replace "\\" "\\\\" . T.replace "\"" "\\\"" . T.replace "\n" "\\n"

-- | Renderiza un diagnóstico en JSON puro sin dependencias externas.
renderDiagnosticJson :: LintDiagnostic -> Text
renderDiagnosticJson d =
  let sevJson = case diagSeverity d of
        SeverityError   -> "\"error\""
        SeverityWarning -> "\"warning\""
        SeverityInfo    -> "\"info\""
      fileJson = maybe "null" (\f -> "\"" <> escapeJson f <> "\"") (locFile (diagLocation d))
      taxonJson = maybe "null" (\t -> "\"" <> escapeJson t <> "\"") (locTaxon (diagLocation d))
      charJson = maybe "null" (T.pack . show) (locCharId (diagLocation d))
  in "{\n"
     <> "    \"code\": \"" <> escapeJson (diagCode d) <> "\",\n"
     <> "    \"severity\": " <> sevJson <> ",\n"
     <> "    \"location\": {\n"
     <> "      \"file\": " <> fileJson <> ",\n"
     <> "      \"taxon\": " <> taxonJson <> ",\n"
     <> "      \"character\": " <> charJson <> "\n"
     <> "    },\n"
     <> "    \"message\": \"" <> escapeJson (diagMessage d) <> "\"\n"
     <> "  }"

-- | Renderiza el informe completo para la terminal con resumen de tiempos.
renderDiagnosticsReportTerminal :: [LintDiagnostic] -> Double -> Text
renderDiagnosticsReportTerminal diags timeMs =
  let errCount = length [() | d <- diags, diagSeverity d == SeverityError]
      warnCount = length [() | d <- diags, diagSeverity d == SeverityWarning]
      infoCount = length [() | d <- diags, diagSeverity d == SeverityInfo]
      formattedTime = T.pack (showFFloat (Just 2) timeMs "") <> " ms"
      header = "\n=======================================================\n"
            <> "          TAXIDE Linter & Semantic Engine             \n"
            <> "=======================================================\n"
      diagLines = if null diags
        then "  [✓] No se detectaron violaciones ontológicas ni errores sintácticos.\n"
        else T.unlines (map (\d -> "  " <> renderDiagnosticTerminal d) diags)
      footer = "\n-------------------------------------------------------\n"
            <> "Resumen: " <> T.pack (show errCount) <> " errores, "
            <> T.pack (show warnCount) <> " avisos, "
            <> T.pack (show infoCount) <> " notas informativas.\n"
            <> "Tiempo de resolución: " <> formattedTime <> "\n"
            <> "======================================================="
  in header <> diagLines <> footer

-- | Renderiza el informe completo en formato JSON estándar.
renderDiagnosticsReportJson :: [LintDiagnostic] -> Double -> Text
renderDiagnosticsReportJson diags timeMs =
  let errCount = length [() | d <- diags, diagSeverity d == SeverityError]
      warnCount = length [() | d <- diags, diagSeverity d == SeverityWarning]
      infoCount = length [() | d <- diags, diagSeverity d == SeverityInfo]
      formattedTime = T.pack (showFFloat (Just 3) timeMs "")
      diagsArr = "[\n  " <> T.intercalate ",\n  " (map renderDiagnosticJson diags) <> "\n  ]"
  in "{\n"
     <> "  \"status\": " <> (if errCount == 0 then "\"ok\"" else "\"failed\"") <> ",\n"
     <> "  \"execution_time_ms\": " <> formattedTime <> ",\n"
     <> "  \"summary\": {\n"
     <> "    \"errors\": " <> T.pack (show errCount) <> ",\n"
     <> "    \"warnings\": " <> T.pack (show warnCount) <> ",\n"
     <> "    \"info\": " <> T.pack (show infoCount) <> "\n"
     <> "  },\n"
     <> "  \"diagnostics\": " <> (if null diags then "[]" else diagsArr) <> "\n"
     <> "}\n"
