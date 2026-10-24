{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit

import Taxide.Core.Invariants (isDependencyTriggered, validateExclusivePolymorphism, validateStatRange)
import Taxide.Core.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Taxide.Core Tests"
  [ testGroup "Dependency Invariants (Subset Trigger Rule)"
      [ testCase "Disables when observed is exact subset of disabling states" $
          isDependencyTriggered (Set.fromList [1]) (Set.fromList [1, 2]) @?= True

      , testCase "Does NOT disable when observed includes enabling state (polymorphism 1/2)" $
          isDependencyTriggered (Set.fromList [1, 2]) (Set.fromList [1]) @?= False

      , testCase "Does NOT disable when observed states are disjoint" $
          isDependencyTriggered (Set.fromList [3]) (Set.fromList [1, 2]) @?= False

      , testCase "Empty observed does not trigger" $
          isDependencyTriggered Set.empty (Set.fromList [1]) @?= False
      ]
  , testGroup "Exclusive Character Validation (EUM/EOM)"
      [ testCase "EUM rejects StateAnd (&)" $
          let expr = StateAnd (SingleState (StateToken 1 Nothing)) (SingleState (StateToken 2 Nothing))
          in validateExclusivePolymorphism EUM expr @?= False

      , testCase "EUM accepts StateOr (/)" $
          let expr = StateOr (SingleState (StateToken 1 Nothing)) (SingleState (StateToken 2 Nothing))
          in validateExclusivePolymorphism EUM expr @?= True

      , testCase "UM accepts StateAnd (&)" $
          let expr = StateAnd (SingleState (StateToken 1 Nothing)) (SingleState (StateToken 2 Nothing))
          in validateExclusivePolymorphism UM expr @?= True
      ]
  , testGroup "StatRange Validation"
      [ testCase "Valid complete range (1-)2-3-4(-5)" $
          let sr = mkFullStatRange (Just 1) 2 (Just 3) 4 (Just 5)
          in validateStatRange (sr :: StatRange Int) @?= True

      , testCase "Invalid range where normalMin > normalMax" $
          let sr = mkRangeStat 5 2
          in validateStatRange (sr :: StatRange Int) @?= False
      ]
  ]
