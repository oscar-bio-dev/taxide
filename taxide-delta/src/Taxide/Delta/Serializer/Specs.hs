{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Delta.Serializer.Specs
Description : Serializador canónico para directivas SPECS de DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Delta.Serializer.Specs
  ( renderSpecs
  ) where

import Data.List (groupBy, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Taxide.Core.Specs (DependentRule (..), ImplicitRule (..), SpecsDef (..))
import Taxide.Core.Types (CharacterType (..))

-- | Renderiza un SpecsDef a texto canónico DELTA formateado.
renderSpecs :: SpecsDef -> Text
renderSpecs s = T.unlines $ concat
  [ [ "*SHOW TAXIDE Canonical SPECS"
    , ""
    , "*NUMBER OF CHARACTERS " <> T.pack (show (specNumCharacters s))
    , "*MAXIMUM NUMBER OF STATES " <> T.pack (show (specMaxNumberStates s))
    , "*MAXIMUM NUMBER OF ITEMS " <> T.pack (show (specMaxNumberItems s))
    , ""
    ]
  , renderCharTypes (specCharTypes s)
  , renderNumbersOfStates (specNumbersOfStates s)
  , renderDependencies (specDependencies s)
  , renderImplicitValues (specImplicitValues s)
  ]

renderCharTypes :: Map.Map Int CharacterType -> [Text]
renderCharTypes m
  | Map.null m = []
  | otherwise =
      let sorted = sortOn fst (Map.toList m)
          grouped = groupBy (\(_, t1) (_, t2) -> t1 == t2) sorted
          chunks = map formatGroup grouped
      in [ "*CHARACTER TYPES"
         , "  " <> T.unwords chunks
         , ""
         ]
  where
    formatGroup [] = ""
    formatGroup entries@((_, ctype):_) =
      let ids = map fst entries
          rangeStr = formatIdRanges ids
      in rangeStr <> "," <> T.pack (show ctype)

renderNumbersOfStates :: Map.Map Int Int -> [Text]
renderNumbersOfStates m
  | Map.null m = []
  | otherwise =
      let sorted = sortOn fst (Map.toList m)
          grouped = groupBy (\(_, n1) (_, n2) -> n1 == n2) sorted
          chunks = map formatGroup grouped
      in [ "*NUMBERS OF STATES"
         , "  " <> T.unwords chunks
         , ""
         ]
  where
    formatGroup [] = ""
    formatGroup entries@((_, n):_) =
      let ids = map fst entries
          rangeStr = formatIdRanges ids
      in rangeStr <> "," <> T.pack (show n)

renderDependencies :: [DependentRule] -> [Text]
renderDependencies [] = []
renderDependencies rules =
  [ "*DEPENDENT CHARACTERS" ]
  ++ map (\r -> "  " <> formatDepRule r) rules
  ++ [ "" ]
  where
    formatDepRule r =
      let c = T.pack (show (depControllerChar r))
          sList = T.intercalate "/" (map (T.pack . show) (Set.toList (depDisablingStates r)))
          dList = formatIdRanges (depDependentChars r)
      in c <> "," <> sList <> ":" <> dList

renderImplicitValues :: [ImplicitRule] -> [Text]
renderImplicitValues [] = []
renderImplicitValues rules =
  [ "*IMPLICIT VALUES"
  , "  " <> T.unwords (map formatRule rules)
  , ""
  ]
  where
    formatRule r =
      let c = T.pack (show (impCharId r))
          s = T.pack (show (impOmitted r))
          unspec = case impUnspecified r of
            Just t  -> ":" <> T.pack (show t)
            Nothing -> ""
      in c <> "," <> s <> unspec

-- | Agrupa listas de enteros consecutivos en notación de rangos: [1,2,3,5] -> "1-3 5".
formatIdRanges :: [Int] -> Text
formatIdRanges [] = ""
formatIdRanges xs =
  let runs = foldr step [] xs
  in T.intercalate " " (map formatRun runs)
  where
    step x [] = [(x, x)]
    step x ((a, b) : rest)
      | x + 1 == a = (x, b) : rest
      | otherwise  = (x, x) : (a, b) : rest

    formatRun (a, b)
      | a == b    = T.pack (show a)
      | otherwise = T.pack (show a) <> "-" <> T.pack (show b)
