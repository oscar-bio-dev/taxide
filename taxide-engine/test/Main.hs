{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Suite completa de pruebas unitarias y basadas en propiedades para taxide-engine
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Main (main) where

import Data.Bits (popCount)
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit hiding (assert)

import Taxide.Core.Character (CharacterDef, mkMultistateChar, mkNumericChar)
import Taxide.Core.Invariants (collectActiveStates)
import Taxide.Core.Item (ItemDef (..), ItemType (..), mkPrimaryItem, mkVariantItem)
import Taxide.Core.Specs (DependentRule (..), ImplicitRule (..), SpecsDef (..), emptySpecs)
import Taxide.Core.Types
  ( CharacterType (..)
  , PseudoValue (..)
  , RawAttribute (..)
  , RawStateExpr (..)
  , StateId
  , StateToken (..)
  , mkRangeStat
  , mkSingleStat
  )
import Taxide.Engine.Dependencies (checkDependencyDAG)
import Taxide.Engine.Diagnostic
  ( DiagnosticLocation (..)
  , EngineError (..)
  , LintDiagnostic (..)
  , Severity (..)
  )
import Taxide.Engine.Discretize
  ( DiscretizeMethod (..)
  , DiscretizeTarget (..)
  , DiscretizedCharacter (..)
  , calculateClassCount
  , discretizeQuantitative
  , discretizeTaxaValues
  )
import Taxide.Engine.Fitch
  ( FitchCharResult (..)
  , evaluateFitchCharacter
  , fitchFinalMask
  , reconstructFitchAncestors
  )
import Taxide.Engine.Gower
  ( buildNeighborJoining
  , buildUPGMA
  , computeGowerMatrix
  , fromDistanceFunction
  , lookupDistance
  )
import Taxide.Engine.Implicits (injectImplicits)
import Taxide.Engine.Key
  ( KeyLang (..)
  , buildIdentificationKey
  , defaultKeyOptions
  , keyLeafTaxa
  , renderBracketedKey
  , renderIndentedKey
  , renderMarkdownKey
  )
import Taxide.Engine.Matrix
  ( ColumnarMatrix (..)
  , isCellInapplicable
  , lookupDiscreteCell
  , tagInapplicable
  , tagValid
  )
import Taxide.Engine.Pipeline (runPipelinePure)
import Taxide.Engine.Sankoff
  ( SankoffCharResult (..)
  , TreeEvaluation (..)
  , evaluateSankoffCharacter
  , evaluateTreeParsimony
  )
import Taxide.Engine.Search
  ( SearchResult (..)
  , searchSPR
  , searchTBR
  , wagnerBuild
  )
import Taxide.Engine.Tree
  ( PhyloTree (..)
  , mkLeaf
  , mkNode
  , renderAsciiTree
  , renderNewick
  , treeLeaves
  )
import Taxide.Engine.Variants (AttrOrigin (..), AttributedState (..), ResolvedTaxon (..), resolveVariants)
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Taxide.Engine Test Suite"
  [ testGroup "Unit Tests: Inferencia y Pipeline de CONFOR en Memoria"
      [ testDallwitzPaineDependencies
      , testVariantsInheritance
      , testImplicitValues
      , testDAGCycleDetection
      , testQuantitativeDiscretization
      , testCladisticAndOntologicalLinter
      , testRegressionFixes
      ]
  , testGroup "Unit Tests: Fase 4 (Fitch, Sankoff, SPR/TBR, Gower, UPGMA/NJ, Claves)"
      [ testFitchAndSankoffParsimony
      , testTopologicalSearchSprTbr
      , testGowerUpgmaAndNj
      , testShannonIdentificationKey
      ]
  , testGroup "Property-Based Tests (Hedgehog): Invariantes Matemáticas, SoA y Fase 4"
      [ testProperty "Invariante DAG: Celda dependiente siempre Inapplicable si controlador dispara" propDagInactivation
      , testProperty "Preservación de dimensiones y SoA en la transposición" propMatrixDimensions
      , testProperty "Gower: D_ii = 0, simetría D_ij = D_ji y acotamiento en [0, 1]" propGowerMetricInvariants
      , testProperty "Clave dicotómica: 100% de taxones distinguibles presentes en las hojas" propKeyCompleteness
      ]
  ]

--------------------------------------------------------------------------------
-- 1. Pruebas Unitarias de Dallwitz & Paine (1986)
--------------------------------------------------------------------------------

testDallwitzPaineDependencies :: TestTree
testDallwitzPaineDependencies = testGroup "Ejemplos Oficiales de Dallwitz & Paine (*DEPENDENT CHARACTERS)"
  [ testCase "Combinación válida 4,2 9,1 10,3: 16 omitido, 9 habilita 10" $ do
      let specs = emptySpecs
            { specNumCharacters = 20
            , specDependencies  =
                [ DependentRule 4 (Set.singleton 2) [16]   -- 4,2:16
                , DependentRule 9 (Set.singleton 2) [10]   -- 9,2:10
                ]
            }
          taxonA = mkPrimaryItem "Taxon_A"
            [ (4, mkState 2)   -- Desactiva 16
            , (9, mkState 1)   -- No desactiva 10 (1 habilita)
            , (10, mkState 3)  -- 10 activo válido
            ]
          (taxa, _, diags) = runPipelinePure specs [taxonA]
          errors = filter (\d -> diagSeverity d == SeverityError) diags

      errors @?= []
      case taxa of
        (t : _) ->
          -- 16 fue forzado automáticamente a Inapplicable al omitirse
          case Map.lookup 16 (rtAttributes t) of
            Just (AttributedState (AttrStates _ (PseudoState Inapplicable _)) _) -> pure ()
            other -> assertFailure ("Se esperaba Inapplicable para 16, pero se obtuvo: " ++ show other)
        [] -> assertFailure "taxa no debe estar vacío"

  , testCase "Polimorfismo parcial 10,1/2 12,1/-: 1 es habilitante por lo que 12 es válido" $ do
      let specs = emptySpecs
            { specNumCharacters = 20
            , specDependencies  =
                [ DependentRule 10 (Set.singleton 2) [12]  -- 10,2:12
                ]
            }
          taxonB = mkPrimaryItem "Taxon_B"
            [ (10, AttrStates Nothing (StateOr (mkStateToken 1) (mkStateToken 2))) -- 10,1/2
            , (12, AttrStates Nothing (StateOr (mkStateToken 1) (PseudoState Inapplicable Nothing))) -- 12,1/-
            ]
          (_, _, diags) = runPipelinePure specs [taxonB]
          errors = filter (\d -> diagSeverity d == SeverityError) diags

      errors @?= []

  , testCase "Contradicción directa 4,2 16,1: 16 está activo cuando 4,2 lo inhabilita" $ do
      let specs = emptySpecs
            { specNumCharacters = 20
            , specDependencies  =
                [ DependentRule 4 (Set.singleton 2) [16]   -- 4,2:16
                ]
            }
          taxonC = mkPrimaryItem "Taxon_C"
            [ (4, mkState 2)   -- Dispara inhabilitación de 16
            , (16, mkState 1)  -- Estado explícito contradictorio!
            ]
          (taxa, mat, diags) = runPipelinePure specs [taxonC]
          depErrors = filter (\d -> diagCode d == "TAX-DEP-002") diags

      length depErrors @?= 1
      -- Verifica que a pesar del error, el estado resuelto fue forzado a Inapplicable
      isCellInapplicable mat 0 16 @?= True
      case taxa of
        (t : _) ->
          case Map.lookup 16 (rtAttributes t) of
            Just (AttributedState (AttrStates _ (PseudoState Inapplicable _)) _) -> pure ()
            other -> assertFailure ("Se esperaba que 16 se forzara a Inapplicable, pero se obtuvo: " ++ show other)
        [] -> assertFailure "taxa no debe estar vacío"

  , testCase "Contradicción en cascada 9,2 10,3 20,1: 9,2 inhabilita 10 y 10,2/3 inhabilita 20" $ do
      let specs = emptySpecs
            { specNumCharacters = 25
            , specDependencies  =
                [ DependentRule 9 (Set.singleton 2) [10]         -- 9,2:10
                , DependentRule 10 (Set.fromList [2, 3]) [20]    -- 10,2/3:20
                ]
            }
          taxonD = mkPrimaryItem "Taxon_D"
            [ (9, mkState 2)   -- Inhabilita 10
            , (10, mkState 3)  -- Contradice 9,2!
            , (20, mkState 1)  -- Puntuado activo
            ]
          (taxaD, _, diags) = runPipelinePure specs [taxonD]
          depErrors = filter (\d -> diagCode d == "TAX-DEP-002") diags

      -- 9 -> 10 y, por clausura transitiva, 10 -> 20: dos contradicciones
      length depErrors @?= 2
      case taxaD of
        (t : _) ->
          case Map.lookup 20 (rtAttributes t) of
            Just (AttributedState (AttrStates _ (PseudoState Inapplicable _)) _) -> pure ()
            other -> assertFailure ("Se esperaba que 20 se propagara a Inapplicable, pero se obtuvo: " ++ show other)
        [] -> assertFailure "taxa no debe estar vacío"
  ]

--------------------------------------------------------------------------------
-- 2. Pruebas Unitarias de Herencia de Variantes (#+)
--------------------------------------------------------------------------------

testVariantsInheritance :: TestTree
testVariantsInheritance = testGroup "Herencia Monoidal de Variantes (#+)"
  [ testCase "Variante hereda atributos del padre y sobrescribe los locales" $ do
      let parent = mkPrimaryItem "Quercus robur"
            [ (1, mkState 1)
            , (2, mkState 1)
            , (3, mkState 1)
            ]
          variant = mkVariantItem "Quercus robur subsp. pedunculiflora"
            [ (2, mkState 2) -- Sobrescribe carácter 2
            ]
          (taxa, diags) = resolveVariants [parent, variant]

      diags @?= []
      length taxa @?= 2
      let v = taxa !! 1
      -- Carácter 1 heredado (estado 1)
      lookupState 1 v @?= Just (Set.singleton 1)
      -- Carácter 2 sobrescrito localmente (estado 2)
      lookupState 2 v @?= Just (Set.singleton 2)
      -- Carácter 3 heredado (estado 1)
      lookupState 3 v @?= Just (Set.singleton 1)

  , testCase "Variante huérfana al inicio del archivo emite TAX-VAR-001" $ do
      let orphan = mkVariantItem "Orphan_Variant" [(1, mkState 1)]
          (_, diags) = resolveVariants [orphan]
          varErrors = filter (\d -> diagCode d == "TAX-VAR-001") diags

      length varErrors @?= 1
  ]

--------------------------------------------------------------------------------
-- 3. Pruebas Unitarias de Valores Implícitos (*IMPLICIT VALUES)
--------------------------------------------------------------------------------

testImplicitValues :: TestTree
testImplicitValues = testGroup "Inyección de Valores Implícitos (*IMPLICIT VALUES)"
  [ testCase "Inyecta estado omitido s en carácter cualitativo ausente" $ do
      let specs = emptySpecs
            { specNumCharacters  = 5
            , specCharTypes      = Map.fromList [(1, UM), (2, UM)]
            , specImplicitValues = [ImplicitRule 1 2 Nothing] -- (1,2)
            }
          taxon = mkPrimaryItem "Taxon_Test"
            [ (2, mkState 1) -- Carácter 1 omitido
            ]
          (taxa, diags) = injectImplicits specs (fst (resolveVariants [taxon]))

      diags @?= []
      case taxa of
        (t : _) -> lookupState 1 t @?= Just (Set.singleton 2)
        []      -> assertFailure "taxa no debe estar vacío"

  , testCase "Emite TAX-IMP-001 si intenta aplicarse a RN o IN" $ do
      let specs = emptySpecs
            { specNumCharacters  = 5
            , specCharTypes      = Map.fromList [(1, RN)]
            , specImplicitValues = [ImplicitRule 1 2 Nothing]
            }
          taxon = mkPrimaryItem "Taxon_Test" []
          (_, diags) = injectImplicits specs (fst (resolveVariants [taxon]))
          impErrors = filter (\d -> diagCode d == "TAX-IMP-001") diags

      length impErrors @?= 1

  , testCase "Valor implícito es sobreescrito por Inapplicable si DAG dispara" $ do
      let specs = emptySpecs
            { specNumCharacters  = 10
            , specCharTypes      = Map.fromList [(1, UM), (2, UM)]
            , specImplicitValues = [ImplicitRule 2 1 Nothing] -- Por defecto 2=1
            , specDependencies   = [DependentRule 1 (Set.singleton 2) [2]] -- 1,2:2
            }
          taxon = mkPrimaryItem "Taxon_Test"
            [ (1, mkState 2) -- Dispara inactivación de 2
            -- 2 está omitido, recibe implícito 2=1 pero debe ser forzado a Inapplicable
            ]
          (taxa, _, diags) = runPipelinePure specs [taxon]
          errors = filter (\d -> diagSeverity d == SeverityError) diags

      -- No debe haber error de contradicción porque era solo un valor implícito
      errors @?= []
      case taxa of
        (t : _) ->
          case Map.lookup 2 (rtAttributes t) of
            Just (AttributedState (AttrStates _ (PseudoState Inapplicable _)) _) -> pure ()
            other -> assertFailure ("Se esperaba Inapplicable para 2, pero se obtuvo: " ++ show other)
        [] -> assertFailure "taxa no debe estar vacío"
  ]

--------------------------------------------------------------------------------
-- 4. Pruebas Unitarias de Ciclos en el DAG
--------------------------------------------------------------------------------

testDAGCycleDetection :: TestTree
testDAGCycleDetection = testGroup "Detección de Ciclos en *DEPENDENT CHARACTERS"
  [ testCase "Ciclo directo 1 -> 2 y 2 -> 1 detectado como TAX-DEP-001" $ do
      let rules =
            [ DependentRule 1 (Set.singleton 2) [2]
            , DependentRule 2 (Set.singleton 2) [1]
            ]
      case checkDependencyDAG rules of
        Left (CyclicDependencyError cycleNodes) ->
          assertBool "El ciclo debe contener caracteres 1 y 2" (1 `elem` cycleNodes && 2 `elem` cycleNodes)
        Left otherErr ->
          assertFailure ("Se esperaba CyclicDependencyError, pero se obtuvo: " ++ show otherErr)
        Right _ ->
          assertFailure "Se esperaba error de ciclo, pero se validó como DAG"
  ]

--------------------------------------------------------------------------------
-- 5. Pruebas Unitarias de Discretización Cuantitativa
--------------------------------------------------------------------------------

testQuantitativeDiscretization :: TestTree
testQuantitativeDiscretization = testGroup "Discretización Cuantitativa (IN/RN -> OM)"
  [ testCase "Regla de la raíz cuadrada y Sturges acotan k en [2, 30]" $ do
      calculateClassCount SquareRootRule 9 @?= 3
      calculateClassCount SquareRootRule 100 @?= 10
      calculateClassCount SturgesRule 10 @?= 5
      calculateClassCount SquareRootRule 2000 @?= 30 -- Acotado al máximo 30

  , testCase "Discretización asigna estados en 1..k correctamente" $ do
      let samples =
            [ (1, mkSingleStat 10.0)
            , (2, mkSingleStat 20.0)
            , (3, mkSingleStat 30.0)
            , (4, mkSingleStat 40.0)
            ]
          dc = discretizeQuantitative 1 (FixedClasses 4) UseCenterVal samples

      dcNumClasses dc @?= 4
      Map.lookup 1 (dcAssignments dc) @?= Just 1
      Map.lookup 4 (dcAssignments dc) @?= Just 4
  ]

--------------------------------------------------------------------------------
-- 6. Pruebas del Linter Cladístico y Ontológico
--------------------------------------------------------------------------------

testCladisticAndOntologicalLinter :: TestTree
testCladisticAndOntologicalLinter = testGroup "Linter Cladístico y Ontológico (TAX-CLAD, TAX-DATA, TAX-MATH, TAX-WARN)"
  [ testCase "TAX-CLAD-001: Carácter invariante emite advertencia Warning" $ do
      let specs = emptySpecs
            { specNumCharacters = 2
            , specCharTypes     = Map.fromList [(1, UM), (2, UM)]
            }
          taxa = [ mkPrimaryItem "Taxon_A" [(1, mkState 1), (2, mkState 1)]
                 , mkPrimaryItem "Taxon_B" [(1, mkState 1), (2, mkState 2)]
                 , mkPrimaryItem "Taxon_C" [(1, mkState 1), (2, mkState 3)]
                 ]
          (_, _, diags) = runPipelinePure specs taxa
          invDiags = filter (\d -> diagCode d == "TAX-CLAD-001") diags

      length invDiags @?= 1
      case invDiags of
        (d : _) -> do
          diagSeverity d @?= SeverityWarning
          locCharId (diagLocation d) @?= Just 1
        [] -> assertFailure "Se esperaba advertencia TAX-CLAD-001"

  , testCase "TAX-CLAD-002: Autapomorfía no informativa emite nota Info" $ do
      let specs = emptySpecs
            { specNumCharacters = 2
            , specCharTypes     = Map.fromList [(1, UM), (2, UM)]
            }
          taxa = [ mkPrimaryItem "Taxon_A" [(1, mkState 1), (2, mkState 1)]
                 , mkPrimaryItem "Taxon_B" [(1, mkState 1), (2, mkState 2)]
                 , mkPrimaryItem "Taxon_C" [(1, mkState 2), (2, mkState 3)]
                 ]
          (_, _, diags) = runPipelinePure specs taxa
          autDiags = filter (\d -> diagCode d == "TAX-CLAD-002") diags

      -- Carácter 1: una única autapomorfía (Taxon_C); carácter 2: tres autapomorfías
      -- en estados distintos (caso generalizado de la Definición 4.1).
      length autDiags @?= 2
      case filter (\d -> locCharId (diagLocation d) == Just 1) autDiags of
        (d : _) -> do
          diagSeverity d @?= SeverityInfo
          locTaxon (diagLocation d) @?= Just "Taxon_C"
        [] -> assertFailure "Se esperaba nota TAX-CLAD-002 para el carácter 1"
      length (filter (\d -> locCharId (diagLocation d) == Just 2) autDiags) @?= 1

  , testCase "TAX-CLAD-002 (generalizado): un estado repetido y varias autapomorfías no es informativo" $ do
      -- Estados: 1,1,2,3 -> sólo el estado 1 aparece en >= 2 taxones.
      let specs = emptySpecs
            { specNumCharacters = 1
            , specCharTypes     = Map.fromList [(1, UM)]
            }
          taxa = [ mkPrimaryItem "A" [(1, mkState 1)]
                 , mkPrimaryItem "B" [(1, mkState 1)]
                 , mkPrimaryItem "C" [(1, mkState 2)]
                 , mkPrimaryItem "D" [(1, mkState 3)]
                 ]
          (_, _, diags) = runPipelinePure specs taxa
      length (filter (\d -> diagCode d == "TAX-CLAD-002") diags) @?= 1

  , testCase "TAX-CLAD-002: un carácter informativo (dos estados en >= 2 taxones) no se reporta" $ do
      let specs = emptySpecs
            { specNumCharacters = 1
            , specCharTypes     = Map.fromList [(1, UM)]
            }
          taxa = [ mkPrimaryItem "A" [(1, mkState 1)]
                 , mkPrimaryItem "B" [(1, mkState 1)]
                 , mkPrimaryItem "C" [(1, mkState 2)]
                 , mkPrimaryItem "D" [(1, mkState 2)]
                 ]
          (_, _, diags) = runPipelinePure specs taxa
      length (filter (\d -> diagCode d == "TAX-CLAD-002") diags) @?= 0

  , testCase "TAX-DATA-001: Carácter 100% no codificado emite advertencia Warning" $ do
      let specs = emptySpecs
            { specNumCharacters = 3
            , specCharTypes     = Map.fromList [(1, UM), (2, UM), (3, UM)]
            }
          taxa = [ mkPrimaryItem "Taxon_A" [(1, mkState 1), (2, mkState 1)]
                 , mkPrimaryItem "Taxon_B" [(1, mkState 2), (2, mkState 2)]
                 ]
          (_, _, diags) = runPipelinePure specs taxa
          uncodedDiags = filter (\d -> diagCode d == "TAX-DATA-001") diags

      length uncodedDiags @?= 1
      case uncodedDiags of
        (d : _) -> do
          diagSeverity d @?= SeverityWarning
          locCharId (diagLocation d) @?= Just 3
        [] -> assertFailure "Se esperaba advertencia TAX-DATA-001"

  , testCase "TAX-DATA-002: Taxones principales duplicados emite advertencia Warning" $ do
      let specs = emptySpecs
            { specNumCharacters = 2
            , specCharTypes     = Map.fromList [(1, UM), (2, UM)]
            }
          itemDupe1 = (mkPrimaryItem "Craspedacusta sowerbyi" [(1, mkState 1), (2, mkState 1)])
            { itemAuthority = Just "Lankester, 1880" }
          itemDupe2 = (mkPrimaryItem "Craspedacusta sowerbyi" [(1, mkState 2), (2, mkState 2)])
            { itemAuthority = Just "Lankester, 1880" }
          (_, _, diags) = runPipelinePure specs [itemDupe1, itemDupe2]
          dupeDiags = filter (\d -> diagCode d == "TAX-DATA-002") diags

      length dupeDiags @?= 1
      case dupeDiags of
        (d : _) -> do
          diagSeverity d @?= SeverityWarning
          locTaxon (diagLocation d) @?= Just "Craspedacusta sowerbyi"
        [] -> assertFailure "Se esperaba advertencia TAX-DATA-002"

  , testCase "TAX-MATH-001: StatRange con normalMin > normalMax emite Error bloqueante" $ do
      let specs = emptySpecs
            { specNumCharacters = 1
            , specCharTypes     = Map.fromList [(1, RN)]
            }
          badRange = mkRangeStat (25.0 :: Double) (10.0 :: Double)
          taxonBad = mkPrimaryItem "Taxon_Bad"
            [ (1, AttrReal Nothing badRange Nothing)
            ]
          (_, _, diags) = runPipelinePure specs [taxonBad]
          mathDiags = filter (\d -> diagCode d == "TAX-MATH-001") diags

      length mathDiags @?= 1
      case mathDiags of
        (d : _) -> do
          diagSeverity d @?= SeverityError
          locTaxon (diagLocation d) @?= Just "Taxon_Bad"
          locCharId (diagLocation d) @?= Just 1
        [] -> assertFailure "Se esperaba error TAX-MATH-001"

  , testCase "TAX-WARN-001: Taxones con idéntico vector de atributos resueltos emiten Warning" $ do
      let specs = emptySpecs
            { specNumCharacters = 2
            , specCharTypes     = Map.fromList [(1, UM), (2, UM)]
            }
          taxa = [ mkPrimaryItem "Taxon_Alpha" [(1, mkState 1), (2, mkState 2)]
                 , mkPrimaryItem "Taxon_Beta"  [(1, mkState 1), (2, mkState 2)]
                 ]
          (_, _, diags) = runPipelinePure specs taxa
          warnDiags = filter (\d -> diagCode d == "TAX-WARN-001") diags

      length warnDiags @?= 1
      case warnDiags of
        (d : _) -> do
          diagSeverity d @?= SeverityWarning
          locTaxon (diagLocation d) @?= Just "Taxon_Alpha / Taxon_Beta"
        [] -> assertFailure "Se esperaba advertencia TAX-WARN-001"
  ]

--------------------------------------------------------------------------------
-- 7. Pruebas Basadas en Propiedades (Hedgehog)
--------------------------------------------------------------------------------

-- Generador de estados discretos
genStateId :: Gen StateId
genStateId = Gen.word8 (Range.linear 1 8)

-- Propiedad: Invariante del DAG en ColumnarMatrix
propDagInactivation :: Property
propDagInactivation = property $ do
  ctrlState <- forAll genStateId
  disablingSt <- forAll genStateId
  let ctrlDisables = ctrlState == disablingSt

  let specs = emptySpecs
        { specNumCharacters = 5
        , specCharTypes     = Map.fromList [(1, UM), (2, UM)]
        , specDependencies  = [DependentRule 1 (Set.singleton disablingSt) [2]]
        }
      taxon = mkPrimaryItem "Taxon_Gen"
        [ (1, mkState ctrlState)
        ]
      (_, mat, _) = runPipelinePure specs [taxon]
      (tag, bitset) = lookupDiscreteCell mat 0 2

  if ctrlDisables
    then do
      -- Si el controlador disparó la regla, el carácter dependiente 2
      -- DEBE ser Inapplicable y tener 0 bits de estado activos.
      tag === tagInapplicable
      popCount bitset === 0
    else do
      -- Si no disparó, al estar omitido es Unknown
      assert (tag /= tagInapplicable)

-- Propiedad: Preservación de dimensiones en la matriz SoA
propMatrixDimensions :: Property
propMatrixDimensions = property $ do
  nTaxa <- forAll $ Gen.int (Range.linear 1 10)
  let items = [ mkPrimaryItem ("Taxon_" <> showT i) [(1, mkState 1)]
              | i <- [1 .. nTaxa]
              ]
      specs = emptySpecs
        { specNumCharacters = 4
        , specCharTypes     = Map.fromList [(1, UM), (2, UM), (3, RN), (4, TE)]
        }
      (taxa, mat, _) = runPipelinePure specs items

  length taxa === nTaxa
  cmNumTaxa mat === nTaxa
  cmNumChars mat === 4
  length (cmTaxonNames mat) === nTaxa

--------------------------------------------------------------------------------
-- 7. Regresiones de defectos corregidos
--------------------------------------------------------------------------------

testRegressionFixes :: TestTree
testRegressionFixes = testGroup "Regresiones de defectos corregidos"
  [ testCase "Cadena transitiva: controlador forzado a '-' inhabilita al dependiente omitido sin error" $ do
      let specs = emptySpecs
            { specNumCharacters = 25
            , specDependencies  =
                [ DependentRule 9 (Set.singleton 2) [10]
                , DependentRule 10 (Set.fromList [2, 3]) [20]
                ]
            }
          taxon = mkPrimaryItem "Taxon_T" [(9, mkState 2)]
          (taxa, _, diags) = runPipelinePure specs [taxon]
      length (filter (\d -> diagCode d == "TAX-DEP-002") diags) @?= 0
      case taxa of
        (t : _) -> do
          let isInap cid = case Map.lookup cid (rtAttributes t) of
                Just (AttributedState (AttrStates _ (PseudoState Inapplicable _)) _) -> True
                _ -> False
          isInap 10 @?= True
          isInap 20 @?= True
        [] -> assertFailure "taxa no debe estar vacío"

  , testCase "IN: los caracteres enteros se discretizan igual que los reales" $ do
      let mkIn :: Int -> Int32 -> ResolvedTaxon
          mkIn i v = ResolvedTaxon
            { rtIndex = i, rtType = PrimaryItem, rtName = "T" <> showT i, rtAuthority = Nothing
            , rtAttributes = Map.fromList [(1, AttributedState (AttrInteger Nothing (mkSingleStat v)) OriginExplicit)]
            , rtComments = []
            }
          taxa = zipWith mkIn [1 ..] [10, 20, 30, 40]
          dc = discretizeTaxaValues 1 (FixedClasses 4) UseCenterVal taxa
      dcNumClasses dc @?= 4
      Map.lookup 1 (dcAssignments dc) @?= Just 1
      Map.lookup 4 (dcAssignments dc) @?= Just 4

  , testCase "TAX-BND-002: un estado fuera de 1..64 se reporta en lugar de descartarse" $ do
      let specs = emptySpecs { specNumCharacters = 1, specCharTypes = Map.fromList [(1, UM)] }
          (_, _, diags) = runPipelinePure specs [mkPrimaryItem "Taxon_X" [(1, mkState 65)]]
      length (filter (\d -> diagCode d == "TAX-BND-002") diags) @?= 1

  , testCase "Capacidad de 64 estados: el estado 64 ocupa el bit 63 de la máscara" $ do
      let specs = emptySpecs { specNumCharacters = 1, specCharTypes = Map.fromList [(1, UM)] }
          (_, mat, diags) = runPipelinePure specs [mkPrimaryItem "Taxon_Y" [(1, mkState 64)]]
      length (filter (\d -> diagCode d == "TAX-BND-002") diags) @?= 0
      popCount (snd (lookupDiscreteCell mat 0 1)) @?= 1

  , testCase "TAX-DATA-003: un carácter repetido en un ítem se reporta" $ do
      let specs = emptySpecs { specNumCharacters = 1, specCharTypes = Map.fromList [(1, UM)] }
          item = mkPrimaryItem "Taxon_Z" [(1, mkState 1), (1, mkState 2)]
          (_, _, diags) = runPipelinePure specs [item]
      length (filter (\d -> diagCode d == "TAX-DATA-003") diags) @?= 1

  , testCase "Clausura transitiva con '-' explícito en controlador intermedio y reglas duplicadas" $ do
      let specs = emptySpecs
            { specNumCharacters = 25
            , specDependencies  =
                [ DependentRule 9 (Set.singleton 2) [10]
                , DependentRule 9 (Set.singleton 3) [10]
                , DependentRule 10 (Set.singleton 2) [20]
                ]
            }
          taxon = mkPrimaryItem "Taxon_ExplicitDash"
            [ (10, AttrStates Nothing (PseudoState Inapplicable Nothing))
            , (20, mkState 1)
            ]
          (taxa, _, diags) = runPipelinePure specs [taxon]
      length (filter (\d -> diagCode d == "TAX-DEP-002") diags) @?= 1
      case taxa of
        (t : _) ->
          case Map.lookup 20 (rtAttributes t) of
            Just (AttributedState (AttrStates _ (PseudoState Inapplicable _)) _) -> pure ()
            other -> assertFailure ("Se esperaba Inapplicable para 20, pero se obtuvo: " ++ show other)
        [] -> assertFailure "taxa no debe estar vacío"
  ]

--------------------------------------------------------------------------------
-- 8. Pruebas Unitarias Fase 4: Fitch, Sankoff, SPR/TBR, Gower/UPGMA/NJ y Claves
--------------------------------------------------------------------------------

testFitchAndSankoffParsimony :: TestTree
testFitchAndSankoffParsimony = testGroup "Parsimonia de Fitch y Sankoff/Wagner e Índices CI/RI/RC"
  [ testCase "Sinapomorfía limpia en 4 taxones ((0,1),(2,3)): s_k=1, m_k=1, g_k=2 => CI=1.0, RI=1.0" $ do
      let specs = emptySpecs
            { specNumCharacters   = 1
            , specMaxNumberStates = 2
            , specCharTypes       = Map.fromList [(1, UM)]
            , specNumbersOfStates = Map.fromList [(1, 2)]
            }
          items =
            [ mkPrimaryItem "T0" [(1, mkState 1)]
            , mkPrimaryItem "T1" [(1, mkState 1)]
            , mkPrimaryItem "T2" [(1, mkState 2)]
            , mkPrimaryItem "T3" [(1, mkState 2)]
            ]
          (taxa, mat, _) = runPipelinePure specs items
          optimalTree    = mkNode (mkNode (mkLeaf 0) (mkLeaf 1)) (mkNode (mkLeaf 2) (mkLeaf 3))
          fitchRes       = evaluateFitchCharacter specs mat 1 optimalTree
          evalRes        = evaluateTreeParsimony SquareRootRule UseCenterVal specs taxa mat optimalTree

      fcrSteps fitchRes @?= 1
      fcrMinSteps fitchRes @?= 1
      fcrMaxSteps fitchRes @?= 2
      assertApprox "Total Length" 1.0 (teTotalLength evalRes)
      assertApprox "CI" 1.0 (teConsistencyIndex evalRes)
      assertApprox "RI" 1.0 (teRetentionIndex evalRes)
      assertApprox "RC" 1.0 (teRescaledCI evalRes)
      assertApprox "HI" 0.0 (teHomoplasyIndex evalRes)

  , testCase "Topología homoplásica ((0,2),(1,3)): s_k=2, m_k=1, g_k=2 => CI=0.5, RI=0.0" $ do
      let specs = emptySpecs
            { specNumCharacters   = 1
            , specMaxNumberStates = 2
            , specCharTypes       = Map.fromList [(1, UM)]
            , specNumbersOfStates = Map.fromList [(1, 2)]
            }
          items =
            [ mkPrimaryItem "T0" [(1, mkState 1)]
            , mkPrimaryItem "T1" [(1, mkState 1)]
            , mkPrimaryItem "T2" [(1, mkState 2)]
            , mkPrimaryItem "T3" [(1, mkState 2)]
            ]
          (taxa, mat, _) = runPipelinePure specs items
          homoplasticTree = mkNode (mkNode (mkLeaf 0) (mkLeaf 2)) (mkNode (mkLeaf 1) (mkLeaf 3))
          fitchRes        = evaluateFitchCharacter specs mat 1 homoplasticTree
          evalRes         = evaluateTreeParsimony SquareRootRule UseCenterVal specs taxa mat homoplasticTree

      fcrSteps fitchRes @?= 2
      fcrMinSteps fitchRes @?= 1
      fcrMaxSteps fitchRes @?= 2
      assertApprox "Total Length" 2.0 (teTotalLength evalRes)
      assertApprox "CI" 0.5 (teConsistencyIndex evalRes)
      assertApprox "RI" 0.0 (teRetentionIndex evalRes)
      assertApprox "RC" 0.0 (teRescaledCI evalRes)
      assertApprox "HI" 0.5 (teHomoplasyIndex evalRes)

  , testCase "Sankoff/Wagner ordenado (OM): transición 1 <-> 3 cuesta 2 pasos y calcula m_k=2, g_k=4" $ do
      let specs = emptySpecs
            { specNumCharacters   = 1
            , specMaxNumberStates = 3
            , specCharTypes       = Map.fromList [(1, OM)]
            , specNumbersOfStates = Map.fromList [(1, 3)]
            }
          items =
            [ mkPrimaryItem "T0" [(1, mkState 1)]
            , mkPrimaryItem "T1" [(1, mkState 1)]
            , mkPrimaryItem "T2" [(1, mkState 3)]
            , mkPrimaryItem "T3" [(1, mkState 3)]
            ]
          (taxa, mat, _) = runPipelinePure specs items
          optTree  = mkNode (mkNode (mkLeaf 0) (mkLeaf 1)) (mkNode (mkLeaf 2) (mkLeaf 3))
          badTree  = mkNode (mkNode (mkLeaf 0) (mkLeaf 2)) (mkNode (mkLeaf 1) (mkLeaf 3))
          sresOpt  = evaluateSankoffCharacter specs mat 1 optTree
          sresBad  = evaluateSankoffCharacter specs mat 1 badTree
          evalOpt  = evaluateTreeParsimony SquareRootRule UseCenterVal specs taxa mat optTree
          evalBad  = evaluateTreeParsimony SquareRootRule UseCenterVal specs taxa mat badTree

      assertApprox "s_k opt" 2.0 (scrSteps sresOpt)
      assertApprox "m_k opt" 2.0 (scrMinSteps sresOpt)
      assertApprox "g_k opt" 4.0 (scrMaxSteps sresOpt)
      assertApprox "CI opt" 1.0 (teConsistencyIndex evalOpt)
      assertApprox "RI opt" 1.0 (teRetentionIndex evalOpt)

      assertApprox "s_k bad" 4.0 (scrSteps sresBad)
      assertApprox "CI bad" 0.5 (teConsistencyIndex evalBad)
      assertApprox "RI bad" 0.0 (teRetentionIndex evalBad)

  , testCase "Segundo pase pre-orden de Fitch reconstruye máscara ancestral y renderiza Newick/ASCII" $ do
      let col  = VU.fromList [1, 1, 2, 2]
          tags = VU.replicate 4 tagValid
          tree = mkNode (mkNode (mkLeaf 0) (mkLeaf 1)) (mkNode (mkLeaf 2) (mkLeaf 3))
          anc  = reconstructFitchAncestors 2 col tags tree
          nwk  = renderNewick (fmap (\i -> "T" <> showT i) tree)
          asc  = renderAsciiTree (fmap (\i -> "T" <> showT i) tree)
      fitchFinalMask anc @?= 3 -- Unión {1, 2} en la raíz
      nwk @?= "((T0,T1),(T2,T3));"
      assertBool "ASCII tree contiene conectores Unicode" ("├── " `T.isInfixOf` asc && "└── " `T.isInfixOf` asc)
  ]

testTopologicalSearchSprTbr :: TestTree
testTopologicalSearchSprTbr = testGroup "Búsqueda Topológica (Wagner, SPR y TBR)"
  [ testCase "Partiendo de una topología subóptima, searchSPR y searchTBR recuperan el árbol óptimo" $ do
      let specs = emptySpecs
            { specNumCharacters   = 4
            , specMaxNumberStates = 2
            , specCharTypes       = Map.fromList [(1, UM), (2, UM), (3, OM), (4, UM)]
            , specNumbersOfStates = Map.fromList [(1, 2), (2, 2), (3, 2), (4, 2)]
            }
          -- Clados verdaderos: (0,1) y (2,3) y (4,5)
          items =
            [ mkPrimaryItem "T0" [(1, mkState 1), (2, mkState 1), (3, mkState 1), (4, mkState 1)]
            , mkPrimaryItem "T1" [(1, mkState 1), (2, mkState 1), (3, mkState 1), (4, mkState 1)]
            , mkPrimaryItem "T2" [(1, mkState 1), (2, mkState 2), (3, mkState 2), (4, mkState 1)]
            , mkPrimaryItem "T3" [(1, mkState 1), (2, mkState 2), (3, mkState 2), (4, mkState 1)]
            , mkPrimaryItem "T4" [(1, mkState 2), (2, mkState 2), (3, mkState 2), (4, mkState 2)]
            , mkPrimaryItem "T5" [(1, mkState 2), (2, mkState 2), (3, mkState 2), (4, mkState 2)]
            ]
          (taxa, mat, _) = runPipelinePure specs items
          -- Topología inicial deliberadamente subóptima (mezclando los clados)
          suboptimalTree =
            mkNode
              (mkNode (mkLeaf 0) (mkNode (mkLeaf 2) (mkLeaf 4)))
              (mkNode (mkLeaf 1) (mkNode (mkLeaf 3) (mkLeaf 5)))
          subEval = evaluateTreeParsimony SquareRootRule UseCenterVal specs taxa mat suboptimalTree
          sprRes  = searchSPR SquareRootRule UseCenterVal specs taxa mat suboptimalTree
          tbrRes  = searchTBR SquareRootRule UseCenterVal specs taxa mat suboptimalTree
          wagTree = wagnerBuild SquareRootRule UseCenterVal specs taxa mat
          wagEval = evaluateTreeParsimony SquareRootRule UseCenterVal specs taxa mat wagTree

      assertBool "El árbol inicial es subóptimo (L > 4)" (teTotalLength subEval > 4.0)
      assertApprox "SPR alcanza longitud mínima L = 4.0" 4.0 (teTotalLength (srEvaluation sprRes))
      assertApprox "SPR alcanza CI = 1.0" 1.0 (teConsistencyIndex (srEvaluation sprRes))
      assertApprox "SPR alcanza RI = 1.0" 1.0 (teRetentionIndex (srEvaluation sprRes))
      assertApprox "TBR alcanza longitud mínima L = 4.0" 4.0 (teTotalLength (srEvaluation tbrRes))
      assertApprox "Wagner alcanza longitud mínima L = 4.0" 4.0 (teTotalLength wagEval)
  ]

testGowerUpgmaAndNj :: TestTree
testGowerUpgmaAndNj = testGroup "Distancia de Gower y Reconstrucción Fenética (UPGMA y NJ)"
  [ testCase "UPGMA reconstruye exactamente la topología y ramas de una matriz ultramétrica de 4 taxones" $ do
      -- Árbol ultramétrico: ((0:0.1, 1:0.1):0.2, (2:0.15, 3:0.15):0.15)
      -- Distancias entre clados: D(0,1) = 0.2, D(2,3) = 0.3, D({0,1},{2,3}) = 0.6
      let distFn i j
            | i == j = 0.0
            | (i == 0 && j == 1) || (i == 1 && j == 0) = 0.2
            | (i == 2 && j == 3) || (i == 3 && j == 2) = 0.3
            | otherwise = 0.6
          dm = fromDistanceFunction 4 distFn
          upgmaTree = buildUPGMA dm

      treeLeaves upgmaTree @?= [0, 1, 2, 3]
      case upgmaTree of
        PhyloNode (PhyloNode (PhyloLeaf 0 (Just b0)) (PhyloLeaf 1 (Just b1)) (Just bL))
                  (PhyloNode (PhyloLeaf 2 (Just b2)) (PhyloLeaf 3 (Just b3)) (Just bR))
                  Nothing -> do
          assertApprox "b0" 0.10 b0
          assertApprox "b1" 0.10 b1
          assertApprox "b2" 0.15 b2
          assertApprox "b3" 0.15 b3
          assertApprox "bL" 0.20 bL
          assertApprox "bR" 0.15 bR
        other -> assertFailure ("Topología UPGMA inesperada: " ++ show other)

  , testCase "Neighbor-Joining reconstruye exactamente un árbol de 4 taxones desde una matriz aditiva" $ do
      -- Árbol aditivo no ultramétrico: ((0:0.1, 1:0.2):0.15, (2:0.12, 3:0.28):0.15)
      -- D(0,1) = 0.30, D(2,3) = 0.40
      -- D(0,2) = 0.1 + 0.3 + 0.12 = 0.52, D(0,3) = 0.1 + 0.3 + 0.28 = 0.68
      -- D(1,2) = 0.2 + 0.3 + 0.12 = 0.62, D(1,3) = 0.2 + 0.3 + 0.28 = 0.78
      let distFn i j =
            let a = min i j
                b = max i j
            in case (a, b) of
                 (0, 1) -> 0.30
                 (2, 3) -> 0.40
                 (0, 2) -> 0.52
                 (0, 3) -> 0.68
                 (1, 2) -> 0.62
                 (1, 3) -> 0.78
                 _      -> 0.0
          dm = fromDistanceFunction 4 distFn
          njTree = buildNeighborJoining dm

      treeLeaves njTree @?= [0, 1, 2, 3]
      case njTree of
        PhyloNode (PhyloNode (PhyloLeaf 0 (Just b0)) (PhyloLeaf 1 (Just b1)) (Just bL))
                  (PhyloNode (PhyloLeaf 2 (Just b2)) (PhyloLeaf 3 (Just b3)) (Just bR))
                  Nothing -> do
          assertApprox "b0" 0.10 b0
          assertApprox "b1" 0.20 b1
          assertApprox "b2" 0.12 b2
          assertApprox "b3" 0.28 b3
          assertApprox "bL + bR" 0.30 (bL + bR)
        other -> assertFailure ("Topología NJ inesperada: " ++ show other)
  ]

testShannonIdentificationKey :: TestTree
testShannonIdentificationKey = testGroup "Generador de Claves Dicotómicas por Entropía de Shannon"
  [ testCase "El 100% de los taxones aparecen en las hojas y se renderizan en Bracketed, Indented y Markdown" $ do
      let specs = emptySpecs
            { specNumCharacters   = 3
            , specMaxNumberStates = 2
            , specCharTypes       = Map.fromList [(1, UM), (2, UM), (3, RN)]
            , specNumbersOfStates = Map.fromList [(1, 2), (2, 2)]
            }
          chars :: [CharacterDef]
          chars =
            [ mkMultistateChar 1 UM "wings" [(1, "present"), (2, "absent")]
            , mkMultistateChar 2 UM "antennae" [(1, "filiform"), (2, "clavate")]
            , mkNumericChar 3 RN "body length" (Just "mm")
            ]
          items =
            [ mkPrimaryItem "Species_A" [(1, mkState 1), (2, mkState 1), (3, AttrReal Nothing (mkSingleStat 5.0) Nothing)]
            , mkPrimaryItem "Species_B" [(1, mkState 1), (2, mkState 2), (3, AttrReal Nothing (mkSingleStat 10.0) Nothing)]
            , mkPrimaryItem "Species_C" [(1, mkState 2), (2, mkState 1), (3, AttrReal Nothing (mkSingleStat 15.0) Nothing)]
            , mkPrimaryItem "Species_D" [(1, mkState 2), (2, mkState 2), (3, AttrReal Nothing (mkSingleStat 20.0) Nothing)]
            ]
          (taxa, mat, _) = runPipelinePure specs items
          keyTree = buildIdentificationKey defaultKeyOptions specs chars taxa mat
          leafSet = Set.fromList (keyLeafTaxa keyTree)
          expectedSet = Set.fromList ["Species_A", "Species_B", "Species_C", "Species_D"]
          txtBrack = renderBracketedKey LangEn keyTree
          txtInd   = renderIndentedKey LangEn keyTree
          txtMdEs  = renderMarkdownKey LangEs keyTree

      leafSet @?= expectedSet
      assertBool "Clave pareada contiene 1(0)." ("1(0)." `T.isInfixOf` txtBrack)
      assertBool "Clave indentada contiene los 4 taxones"
        (all (`T.isInfixOf` txtInd) ["Species_A", "Species_B", "Species_C", "Species_D"])
      assertBool "Clave Markdown en español contiene título"
        ("# Clave de Identificación" `T.isInfixOf` txtMdEs)
  ]

--------------------------------------------------------------------------------
-- 9. Propiedades Hedgehog de Fase 4 (Gower y Claves)
--------------------------------------------------------------------------------

-- | Propiedad: Para toda matriz resuelta, la distancia de Gower cumple:
-- D_ii = 0, D_ij = D_ji y D_ij in [0, 1].
propGowerMetricInvariants :: Property
propGowerMetricInvariants = property $ do
  nTaxa <- forAll $ Gen.int (Range.linear 2 8)
  st1   <- forAll $ Gen.list (Range.singleton nTaxa) (Gen.word8 (Range.linear 1 4))
  st2   <- forAll $ Gen.list (Range.singleton nTaxa) (Gen.word8 (Range.linear 1 4))
  vals3 <- forAll $ Gen.list (Range.singleton nTaxa) (Gen.double (Range.linearFrac 1.0 50.0))

  let specs = emptySpecs
        { specNumCharacters   = 3
        , specMaxNumberStates = 4
        , specCharTypes       = Map.fromList [(1, UM), (2, OM), (3, RN)]
        , specNumbersOfStates = Map.fromList [(1, 4), (2, 4)]
        }
      items =
        [ mkPrimaryItem ("T" <> showT idx)
            [ (1, mkState s1)
            , (2, mkState s2)
            , (3, AttrReal Nothing (mkSingleStat v3) Nothing)
            ]
        | (idx, s1, s2, v3) <- zip4 [1 .. nTaxa] st1 st2 vals3
        ]
      (_, mat, _) = runPipelinePure specs items
      dm = computeGowerMatrix specs mat

  forM_ [0 .. nTaxa - 1] $ \i -> do
    let dII = lookupDistance dm i i
    assert (abs dII <= 1.0e-12)
    forM_ [0 .. nTaxa - 1] $ \j -> do
      let dIJ = lookupDistance dm i j
          dJI = lookupDistance dm j i
      assert (abs (dIJ - dJI) <= 1.0e-12)
      assert (dIJ >= 0.0 && dIJ <= 1.0)
  where
    zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4 as bs cs ds
    zip4 _ _ _ _ = []
    forM_ xs f = sequence_ (map f xs)

-- | Propiedad: Para todo dataset con taxones distinguibles, el 100% de los taxones
-- aparecen en las hojas de la clave dicotómica generada.
propKeyCompleteness :: Property
propKeyCompleteness = property $ do
  nTaxa <- forAll $ Gen.int (Range.linear 2 8)
  -- Codificamos cada taxón i (0..nTaxa-1) con sus bits binarios en 3 caracteres UM
  -- garantizando que todos los vectores de estados sean mutuamente distinguibles
  let bitState idx b = if (idx `div` (2 ^ b)) `mod` 2 == 0 then 1 else 2
      specs = emptySpecs
        { specNumCharacters   = 3
        , specMaxNumberStates = 2
        , specCharTypes       = Map.fromList [(1, UM), (2, UM), (3, UM)]
        , specNumbersOfStates = Map.fromList [(1, 2), (2, 2), (3, 2)]
        }
      chars =
        [ mkMultistateChar 1 UM "c1" [(1, "s1"), (2, "s2")]
        , mkMultistateChar 2 UM "c2" [(1, "s1"), (2, "s2")]
        , mkMultistateChar 3 UM "c3" [(1, "s1"), (2, "s2")]
        ]
      items =
        [ mkPrimaryItem ("Taxon_" <> showT i)
            [ (1, mkState (bitState i (0 :: Int)))
            , (2, mkState (bitState i (1 :: Int)))
            , (3, mkState (bitState i (2 :: Int)))
            ]
        | i <- [0 .. nTaxa - 1]
        ]
      (taxa, mat, _) = runPipelinePure specs items
      keyTree = buildIdentificationKey defaultKeyOptions specs chars taxa mat
      expected = Set.fromList ["Taxon_" <> showT i | i <- [0 .. nTaxa - 1]]
      actual   = Set.fromList (keyLeafTaxa keyTree)

  actual === expected

assertApprox :: String -> Double -> Double -> Assertion
assertApprox lbl expected actual =
  assertBool
    (lbl ++ ": se esperaba " ++ show expected ++ " pero se obtuvo " ++ show actual)
    (abs (expected - actual) <= 1.0e-6)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

showT :: Show a => a -> T.Text
showT = T.pack . show

mkState :: StateId -> RawAttribute
mkState s = AttrStates Nothing (SingleState (StateToken s Nothing))

mkStateToken :: StateId -> RawStateExpr
mkStateToken s = SingleState (StateToken s Nothing)

lookupState :: Int -> ResolvedTaxon -> Maybe (Set StateId)
lookupState cid taxon = do
  AttributedState raw _ <- Map.lookup cid (rtAttributes taxon)
  case raw of
    AttrStates _ expr -> Just (collectActiveStates expr)
    _                 -> Nothing
