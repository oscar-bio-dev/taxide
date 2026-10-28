{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Engine.Discretize
Description : Discretización cuantitativa dinámica en memoria (IN/RN -> OM)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Engine.Discretize
  ( DiscretizeMethod (..)
  , DiscretizeTarget (..)
  , DiscretizedCharacter (..)
  , calculateClassCount
  , extractValue
  , discretizeQuantitative
  , discretizeTaxaValues
  ) where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Taxide.Core.Types (RawAttribute (..), StatRange (..), StateId)
import Taxide.Engine.Variants (AttributedState (..), ResolvedTaxon (..))

-- | Método estadístico para estimar el número óptimo de clases/intervalos (k).
data DiscretizeMethod
  = SquareRootRule
  -- ^ Regla de la raíz cuadrada: k = ceil(sqrt(N))
  | SturgesRule
  -- ^ Regla de Sturges: k = ceil(1 + 3.322 * log10(N))
  | FixedClasses !Int
  -- ^ Número fijo predefinido de clases
  deriving (Eq, Show, Read, Generic, NFData)

-- | Estrategia de selección del valor escalar representativo de un 'StatRange'.
data DiscretizeTarget
  = UseCenterVal
  -- ^ Prioriza el valor central (-med-) si existe; fallback al punto medio normal
  | UseNormalMidpoint
  -- ^ Utiliza estrictamente la media aritmética del rango normal: (n1 + n2) / 2
  deriving (Eq, Show, Read, Generic, NFData)

-- | Resultado de la discretización para un carácter cuantitativo específico.
data DiscretizedCharacter = DiscretizedCharacter
  { dcCharId      :: !Int
  -- ^ Identificador del carácter cuantitativo original
  , dcNumClasses  :: !Int
  -- ^ Número de estados/clases ordenadas k calculadas (acotado entre 2 y 30)
  , dcBoundaries  :: ![Double]
  -- ^ Puntos de corte de los intervalos [b0, b1, ..., bk]
  , dcAssignments :: !(Map Int StateId)
  -- ^ Asignación por taxón (TaxonIndex -> Estado ordenado 1..k)
  } deriving (Eq, Show, Read, Generic, NFData)

-- | Calcula el número de intervalos de clase k según la regla estadística,
-- acotando el resultado en el rango biológicamente admisible [2, 30].
calculateClassCount :: DiscretizeMethod -> Int -> Int
calculateClassCount method n
  | n <= 1 = 1
  | otherwise =
      let nD = fromIntegral n :: Double
          rawK = case method of
            SquareRootRule  -> ceiling (sqrt nD)
            SturgesRule     -> ceiling (1.0 + 3.322 * logBase 10.0 nD)
            FixedClasses fc -> fc
      in max 2 (min 30 rawK)

-- | Extrae el valor escalar representativo a partir de un 'StatRange Double'.
extractValue :: DiscretizeTarget -> StatRange Double -> Double
extractValue target sr = case target of
  UseCenterVal ->
    case srCenterVal sr of
      Just c  -> c
      Nothing -> (srNormalMin sr + srNormalMax sr) / 2.0
  UseNormalMidpoint ->
    (srNormalMin sr + srNormalMax sr) / 2.0

-- | Discretiza una lista de pares (TaxonIndex, StatRange) en k clases ordenadas.
discretizeQuantitative
  :: Int
  -> DiscretizeMethod
  -> DiscretizeTarget
  -> [(Int, StatRange Double)]
  -> DiscretizedCharacter
discretizeQuantitative cid method target samples
  | null samples =
      DiscretizedCharacter cid 0 [] Map.empty
  | otherwise =
      let extracted = [(tIdx, extractValue target sr) | (tIdx, sr) <- samples]
          vals = map snd extracted
          vMin = minimum vals
          vMax = maximum vals
          k = calculateClassCount method (length samples)
      in if vMin == vMax
           then
             -- Carácter invariante sin variación métrica
             let assigns = Map.fromList [(tIdx, 1) | (tIdx, _) <- extracted]
             in DiscretizedCharacter cid 1 [vMin, vMax] assigns
           else
             let kD = fromIntegral k :: Double
                 width = (vMax - vMin) / kD
                 boundaries = [vMin + fromIntegral i * width | i <- [0 .. k]]
                 assigns = Map.fromList
                   [ (tIdx, assignClass v vMin width k)
                   | (tIdx, v) <- extracted
                   ]
             in DiscretizedCharacter cid k boundaries assigns
  where
    assignClass v vMin width k =
      let rawIdx = floor ((v - vMin) / width)
          clamped = max 0 (min (k - 1) rawIdx)
      in fromIntegral (1 + clamped)

-- | Discretiza un carácter cuantitativo a lo largo de una colección de taxones resueltos.
discretizeTaxaValues
  :: Int
  -> DiscretizeMethod
  -> DiscretizeTarget
  -> [ResolvedTaxon]
  -> DiscretizedCharacter
discretizeTaxaValues cid method target taxa =
  let samples = [ (rtIndex t, sr)
                | t <- taxa
                , Just (AttributedState attr _) <- [Map.lookup cid (rtAttributes t)]
                , Just sr <- [quantitativeRange attr]
                ]
  in discretizeQuantitative cid method target samples

-- | Extrae el rango cuantitativo de un atributo real (@RN@) o entero (@IN@),
-- normalizando ambos a @StatRange Double@.
quantitativeRange :: RawAttribute -> Maybe (StatRange Double)
quantitativeRange (AttrReal _ sr _) = Just sr
quantitativeRange (AttrInteger _ sr) = Just (fmap' sr)
  where
    fmap' r = StatRange
      { srAbsoluteMin = fromIntegral <$> srAbsoluteMin r
      , srNormalMin   = fromIntegral (srNormalMin r)
      , srCenterVal   = fromIntegral <$> srCenterVal r
      , srNormalMax   = fromIntegral (srNormalMax r)
      , srAbsoluteMax = fromIntegral <$> srAbsoluteMax r
      }
quantitativeRange _ = Nothing
