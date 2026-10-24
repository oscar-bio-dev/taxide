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

import Taxide.Core.Invariants (collectActiveStates)
import Taxide.Core.Item (mkPrimaryItem, mkVariantItem)
import Taxide.Core.Specs (DependentRule (..), ImplicitRule (..), SpecsDef (..), emptySpecs)
import Taxide.Core.Types
  ( CharacterType (..)
  , PseudoValue (..)
  , RawAttribute (..)
  , RawStateExpr (..)
  , StateId
  , StateToken (..)
  , mkSingleStat
  )
import Taxide.Engine.Dependencies (checkDependencyDAG)
import Taxide.Engine.Diagnostic (EngineError (..), LintDiagnostic (..), Severity (..))
import Taxide.Engine.Discretize
  ( DiscretizeMethod (..)
  , DiscretizeTarget (..)
  , DiscretizedCharacter (..)
  , calculateClassCount
  , discretizeQuantitative
  )
import Taxide.Engine.Implicits (injectImplicits)
import Taxide.Engine.Matrix
  ( ColumnarMatrix (..)
  , isCellInapplicable
  , lookupDiscreteCell
  , tagInapplicable
  )
import Taxide.Engine.Pipeline (runPipelinePure)
import Taxide.Engine.Variants (AttributedState (..), ResolvedTaxon (..), resolveVariants)

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
      ]
  , testGroup "Property-Based Tests (Hedgehog): Invariantes Matemáticas y SoA"
      [ testProperty "Invariante DAG: Celda dependiente siempre Inapplicable si controlador dispara" propDagInactivation
      , testProperty "Preservación de dimensiones y SoA en la transposición" propMatrixDimensions
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
          (_, _, diags) = runPipelinePure specs [taxonD]
          depErrors = filter (\d -> diagCode d == "TAX-DEP-002") diags

      -- 10 contradice a 9
      assertBool "Debe detectarse contradicción TAX-DEP-002" (not (null depErrors))
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
-- 6. Pruebas Basadas en Propiedades (Hedgehog)
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
