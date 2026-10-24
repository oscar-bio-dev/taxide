{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Suite de pruebas de propiedad (Hedgehog) y pruebas unitarias para taxide-delta
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import Text.Megaparsec (runParser)

import Taxide.Core.Character (CharacterDef (..))
import Taxide.Core.Item (ItemDef (..), ItemType (..))
import Taxide.Core.Specs (DependentRule (..), ImplicitRule (..), SpecsDef (..))
import Taxide.Core.Types
import Taxide.Delta.Parser.Chars (parseCharsText)
import Taxide.Delta.Parser.Items (pStateExpr, pStatRange, parseItemsText)
import Taxide.Delta.Parser.Lexer (pInteger, pNestedComment)
import Taxide.Delta.Parser.Specs (parseSpecsText)
import Taxide.Delta.Serializer.Chars (renderChars)
import Taxide.Delta.Serializer.Items (renderAttribute, renderItems, renderStateExpr, renderStatRange)
import Taxide.Delta.Serializer.Specs (renderSpecs)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Taxide.Delta Test Suite"
  [ unitTests
  , propertyTests
  ]

--------------------------------------------------------------------------------
-- Pruebas Unitarias (Golden & Edge Cases)
--------------------------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "Unit & Golden Tests"
  [ testCase "Nested comments parse arbitrarily deep: <a <b <c>>>" $ do
      let input = "<hojas <raramente <muy> lobuladas> pubescentes>"
      case runParser pNestedComment "test" input of
        Left err  -> assertFailure (show err)
        Right res -> res @?= "hojas <raramente <muy> lobuladas> pubescentes"

  , testCase "Ambiguous contextual slash '/' does not break literal text or OR" $ do
      let input = "1/2"
      case runParser pStateExpr "test" input of
        Left err   -> assertFailure (show err)
        Right expr -> expr @?= StateOr (SingleState (StateToken 1 Nothing)) (SingleState (StateToken 2 Nothing))

  , testCase "Attribute parser and renderer handles quantitative with extremes" $ do
      let attr = AttrInteger (Just "notes") (mkFullStatRange (Just 1) 2 (Just 3) 4 (Just 5))
          rendered = renderAttribute attr
      rendered @?= "<notes>(1-)2-3-4(-5)"

  , testCase "Full SPECS round-trip with dependencies and implicit values" $ do
      let specs = SpecsDef
            { specNumCharacters   = 5
            , specMaxNumberStates = 4
            , specMaxNumberItems  = 10
            , specCharTypes       = Map.fromList [(1, UM), (2, OM), (3, IN), (4, RN), (5, TE)]
            , specNumbersOfStates = Map.fromList [(1, 3), (2, 4)]
            , specDependencies    = [DependentRule 1 (Set.fromList [1]) [2, 3]]
            , specImplicitValues  = [ImplicitRule 2 1 (Just 2)]
            }
          rendered = renderSpecs specs
      case parseSpecsText "specs" rendered of
        Left err   -> assertFailure (T.unpack err)
        Right parsed -> do
          specNumCharacters parsed @?= 5
          specMaxNumberStates parsed @?= 4
          specCharTypes parsed @?= specCharTypes specs
          specDependencies parsed @?= specDependencies specs

  , testCase "CHARS definitions round-trip" $ do
      let chars =
            [ CharacterDef 1 UM (Just "habit") "plant habit" Nothing
                [(1, "herbaceous"), (2, "woody")] []
            , CharacterDef 2 RN Nothing "leaf length" (Just "cm")
                [] []
            ]
          rendered = renderChars chars
      case parseCharsText (Map.fromList [(1, UM), (2, RN)]) "chars" rendered of
        Left err     -> assertFailure (T.unpack err)
        Right parsed -> case parsed of
          [c1, c2] -> do
            charNumber c1 @?= 1
            charFeature c1 @?= "plant habit"
            charStates c1 @?= [(1, "herbaceous"), (2, "woody")]
            charUnits c2 @?= Just "cm"
          other -> assertFailure ("Se esperaban 2 caracteres, pero se obtuvo: " ++ show (length other))

  , testCase "ITEMS descriptions round-trip and itemAuthority extraction" $ do
      let items =
            [ ItemDef PrimaryItem "Craspedacusta sowerbyi <Lankester, 1880>" (Just "Lankester, 1880")
                [ (1, AttrStates Nothing (SingleState (StateToken 1 Nothing)))
                , (2, AttrStates Nothing (StateOr (SingleState (StateToken 1 Nothing)) (SingleState (StateToken 2 Nothing))))
                , (3, AttrStates Nothing (PseudoState Inapplicable Nothing))
                ] []
            , ItemDef VariantItem "Craspedacusta sowerbyi var. kiangsuensis" Nothing
                [ (1, AttrStates Nothing (SingleState (StateToken 2 (Just "rare"))))
                ] []
            ]
          rendered = renderItems items
      case parseItemsText (Map.fromList [(1, UM), (2, UM), (3, UM)]) "items" rendered of
        Left err     -> assertFailure (T.unpack err)
        Right parsed -> case parsed of
          [i1, i2] -> do
            itemType i1 @?= PrimaryItem
            itemName i1 @?= "Craspedacusta sowerbyi <Lankester, 1880>"
            itemAuthority i1 @?= Just "Lankester, 1880"
            itemType i2 @?= VariantItem
            itemAuthority i2 @?= Nothing
          other -> assertFailure ("Se esperaban 2 ítems, pero se obtuvo: " ++ show (length other))
  ]

--------------------------------------------------------------------------------
-- Pruebas Basadas en Propiedades (Hedgehog)
--------------------------------------------------------------------------------

propertyTests :: TestTree
propertyTests = testGroup "Property-Based Tests (Hedgehog)"
  [ testProperty "Propiedad Round-Trip: StatRange Int" prop_roundtrip_statrange
  , testProperty "Propiedad Round-Trip: RawStateExpr" prop_roundtrip_state_expr
  ]

-- | Generador de texto limpio sin caracteres delimitadores de DELTA.
genSafeText :: Gen Text
genSafeText = Gen.text (Range.linear 1 12) Gen.alphaNum

-- | Generador de tokens de estado con comentarios opcionales.
genStateToken :: Gen StateToken
genStateToken = do
  sid <- Gen.word8 (Range.linear 1 10)
  mCom <- Gen.maybe genSafeText
  pure (StateToken sid mCom)

-- | Generador de expresiones de estados asociativas por la izquierda según la gramática DELTA.
genRawStateExpr :: Gen RawStateExpr
genRawStateExpr = do
  andTerms <- Gen.list (Range.linear 1 3) genAndTerm
  pure (foldl1 StateOr andTerms)
  where
    genAndTerm = do
      atoms <- Gen.list (Range.linear 1 3) genAtom
      pure (foldl1 StateAnd atoms)

    genAtom = Gen.choice
      [ SingleState <$> genStateToken
      , RangeTo <$> genStateToken <*> genStateToken
      , PseudoState <$> Gen.element [Unknown, Inapplicable, Variable] <*> Gen.maybe genSafeText
      ]

-- | Generador de rangos estadísticos (min-)n1-med-n2(-max).
genStatRangeInt :: Gen (StatRange Int)
genStatRangeInt = do
  n1 <- Gen.int (Range.linear 10 20)
  n2 <- Gen.int (Range.linear 20 40)
  mMed <- Gen.maybe (Gen.int (Range.linear n1 n2))
  mAbsMin <- Gen.maybe (Gen.int (Range.linear 1 (n1 - 1)))
  mAbsMax <- Gen.maybe (Gen.int (Range.linear (n2 + 1) 100))
  pure $ StatRange mAbsMin n1 mMed n2 mAbsMax

-- | Propiedad de Round-Trip para rangos cuantitativos.
prop_roundtrip_statrange :: Property
prop_roundtrip_statrange = property $ do
  sr <- forAll genStatRangeInt
  let rendered = renderStatRange (T.pack . show) sr
  case runParser (pStatRange pInteger) "test" rendered of
    Left err -> do
      annotate (show err)
      failure
    Right parsed ->
      parsed === sr

-- | Propiedad de Round-Trip para expresiones de estados morfológicos.
prop_roundtrip_state_expr :: Property
prop_roundtrip_state_expr = property $ do
  expr <- forAll genRawStateExpr
  let rendered = renderStateExpr expr
  case runParser pStateExpr "test" rendered of
    Left err -> do
      annotate ("Rendered: " ++ T.unpack rendered)
      annotate ("Parse Error: " ++ show err)
      failure
    Right parsed ->
      parsed === expr
