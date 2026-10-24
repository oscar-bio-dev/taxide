{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Engine.Pipeline
Description : Orquestador puro y determinista del pipeline semántico de TAXIDE
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Engine.Pipeline
  ( PipelineResult (..)
  , runPipeline
  , runPipelinePure
  , validateInvariants
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)

import Taxide.Core.Invariants
  ( validateExclusivePolymorphism
  , validateStatRange
  , validateStateBounds
  )
import Taxide.Core.Item (ItemDef)
import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Core.Types
  ( CharacterType (..)
  , RawAttribute (..)
  )
import Taxide.Engine.Dependencies (propagateDependencies)
import Taxide.Engine.Diagnostic
  ( LintDiagnostic (..)
  , Severity (..)
  , taxonCharLoc
  )
import Taxide.Engine.Implicits (injectImplicits)
import Taxide.Engine.Matrix (ColumnarMatrix, buildColumnarMatrix)
import Taxide.Engine.Variants (AttributedState (..), ResolvedTaxon (..), resolveVariants)

-- | Resultado integral de la ejecución del pipeline semántico.
data PipelineResult = PipelineResult
  { prTaxa            :: ![ResolvedTaxon]
  -- ^ Colección de taxones completamente resueltos
  , prMatrix          :: !ColumnarMatrix
  -- ^ Matriz columnar Structure of Arrays (SoA)
  , prDiagnostics     :: ![LintDiagnostic]
  -- ^ Lista consolidada de diagnósticos y violaciones del linter
  , prExecutionTimeMs :: !Double
  -- ^ Tiempo de ejecución cronometrado en milisegundos
  } deriving (Eq, Show, Generic, NFData)

-- | Ejecuta el pipeline completo midiendo el tiempo de resolución en milisegundos.
runPipeline :: SpecsDef -> [ItemDef] -> IO PipelineResult
runPipeline specs items = do
  t0 <- getMonotonicTime
  let (taxa, matrix, diags) = runPipelinePure specs items
  t1 <- getMonotonicTime
  let elapsedMs = (t1 - t0) * 1000.0
  pure PipelineResult
    { prTaxa            = taxa
    , prMatrix          = matrix
    , prDiagnostics     = diags
    , prExecutionTimeMs = elapsedMs
    }

-- | Ejecución puramente funcional y determinista del pipeline en 5 pasos:
-- 1. Herencia monoidal de variantes (#+)
-- 2. Inyección de valores implícitos (*IMPLICIT VALUES)
-- 3. Compilación y propagación del DAG de dependencias (*DEPENDENT CHARACTERS)
-- 4. Validación de invariantes biológicas (exclusión, rangos, cotas)
-- 5. Transposición a matriz columnar SoA
runPipelinePure :: SpecsDef -> [ItemDef] -> ([ResolvedTaxon], ColumnarMatrix, [LintDiagnostic])
runPipelinePure specs items =
  -- Paso 1: Herencia de variantes (#+)
  let (taxaStep1, diagsStep1) = resolveVariants items

      -- Paso 2: Inyección de valores implícitos
      (taxaStep2, diagsStep2) = injectImplicits specs taxaStep1

      -- Paso 3: Propagación del DAG de dependencias
      (taxaStep3, diagsStep3) = propagateDependencies specs taxaStep2

      -- Paso 4: Validación de invariantes adicionales
      diagsInvariants = validateInvariants specs taxaStep3

      -- Consolidación de todos los diagnósticos
      allDiags = diagsStep1 ++ diagsStep2 ++ diagsStep3 ++ diagsInvariants

      -- Paso 5: Transposición a matriz SoA
      soaMatrix = buildColumnarMatrix specs taxaStep3

  in (taxaStep3, soaMatrix, allDiags)

-- | Valida invariantes ontológicas sobre los caracteres resueltos:
-- - Exclusividad EUM/EOM: prohíbe el operador '&' (StateAnd)
-- - Coherencia métrica en StatRange: min <= normalMin <= normalMax <= max
-- - Límites de estados: los estados no deben superar specMaxNumberStates
-- - Caracteres no codificados: emite aviso informativo (TAX-UNC-004)
validateInvariants :: SpecsDef -> [ResolvedTaxon] -> [LintDiagnostic]
validateInvariants specs taxa = concatMap checkTaxon taxa
  where
    maxStates = specMaxNumberStates specs
    numChars = specNumCharacters specs
    typeMap = specCharTypes specs

    checkTaxon taxon =
      let tName = rtName taxon
          attrs = rtAttributes taxon

          -- Chequeo de atributos presentes
          attrDiags = concatMap (checkAttribute tName typeMap maxStates) (Map.toList attrs)

          -- Chequeo de caracteres no codificados (uncoded)
          uncodedDiags =
            if numChars > 0
              then [ LintDiagnostic
                       { diagCode     = "TAX-UNC-004"
                       , diagSeverity = SeverityInfo
                       , diagLocation = taxonCharLoc tName cid
                       , diagMessage  = "El taxón '" <> tName <> "' no tiene codificado el carácter " <> T.pack (show cid) <> "."
                       }
                   | cid <- [1 .. numChars]
                   , Map.notMember cid attrs
                   ]
              else []

      in attrDiags ++ uncodedDiags

    checkAttribute tName types maxSt (cid, AttributedState raw _) =
      let mType = Map.lookup cid types
          loc = taxonCharLoc tName cid
      in case raw of
        AttrStates _ expr ->
          let exclusiveDiag = case mType of
                Just EUM | not (validateExclusivePolymorphism EUM expr) ->
                  [ LintDiagnostic
                      { diagCode     = "TAX-TYPE-001"
                      , diagSeverity = SeverityError
                      , diagLocation = loc
                      , diagMessage  = "Violación de exclusividad: el carácter " <> T.pack (show cid)
                                    <> " es EUM (Exclusivo Desordenado) y no admite polimorfismo simultáneo '&'."
                      }
                  ]
                Just EOM | not (validateExclusivePolymorphism EOM expr) ->
                  [ LintDiagnostic
                      { diagCode     = "TAX-TYPE-001"
                      , diagSeverity = SeverityError
                      , diagLocation = loc
                      , diagMessage  = "Violación de exclusividad: el carácter " <> T.pack (show cid)
                                    <> " es EOM (Exclusivo Ordenado) y no admite polimorfismo simultáneo '&'."
                      }
                  ]
                _ -> []

              boundsDiag =
                if maxSt > 0 && not (validateStateBounds maxSt expr)
                  then [ LintDiagnostic
                           { diagCode     = "TAX-BND-001"
                           , diagSeverity = SeverityError
                           , diagLocation = loc
                           , diagMessage  = "Identificador de estado fuera de rango para el carácter " <> T.pack (show cid)
                                         <> "; el máximo declarado es " <> T.pack (show maxSt) <> "."
                           }
                       ]
                  else []

          in exclusiveDiag ++ boundsDiag

        AttrReal _ sr _ ->
          if not (validateStatRange sr)
            then [ LintDiagnostic
                     { diagCode     = "TAX-RNG-001"
                     , diagSeverity = SeverityError
                     , diagLocation = loc
                     , diagMessage  = "Rango cuantitativo inconsistente en el carácter " <> T.pack (show cid)
                                   <> ": no se cumple min <= normalMin <= normalMax <= max."
                     }
                 ]
            else []

        AttrInteger _ sr ->
          if not (validateStatRange sr)
            then [ LintDiagnostic
                     { diagCode     = "TAX-RNG-001"
                     , diagSeverity = SeverityError
                     , diagLocation = loc
                     , diagMessage  = "Rango entero inconsistente en el carácter " <> T.pack (show cid)
                                   <> ": no se cumple min <= normalMin <= normalMax <= max."
                     }
                 ]
            else []

        AttrText _ _ -> []
