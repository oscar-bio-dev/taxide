{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Engine.Gower
Description : Coeficiente general de similitud de Gower y reconstrucción fenética (UPGMA y NJ)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Implementa las ecuaciones de las Secciones 5.3 y 5.5 de la especificación formal:
1. Matriz de similitud general de Gower $S_{ij}$ y distancias derivadas $D_{ij} = 1 - S_{ij}$
   y $D^{\text{E}}_{ij} = \sqrt{1 - S_{ij}}$, combinando Jaccard a nivel de bits para
   @UM@/@EUM@, extensión ordinal de Podani (1999) para @OM@/@EOM@ y normalización por rango
   para @IN@/@RN@, enmascarando ausencias con $w_{ijk} = 0$.
2. Reconstrucción de fenogramas con longitudes de rama mediante UPGMA (Sokal & Michener, 1958)
   y Neighbor-Joining (Saitou & Nei, 1987).
-}

module Taxide.Engine.Gower
  ( -- * Matriz de Similitud y Distancia de Gower
    DistanceMatrix (..)
  , computeGowerMatrix
  , fromDistanceFunction
  , lookupSimilarity
  , lookupDistance
  , lookupEuclideanDistance

    -- * Reconstrucción Fenética con Longitudes de Rama
  , buildUPGMA
  , buildNeighborJoining
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (popCount, testBit, (.&.), (.|.))
import Data.List (minimumBy)
#if __GLASGOW_HASKELL__ < 910
import Data.List (foldl')
#endif
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import GHC.Generics (Generic)

import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Core.Types (CharacterType (..))
import Taxide.Engine.Matrix
  ( ColumnarMatrix (..)
  , NumericColumn (..)
  , PseudoTag
  , maxBitsetStates
  , tagValid
  )
import Taxide.Engine.Tree (PhyloTree (..), setBranchLength)

-- | Matriz simétrica $N \times N$ de similitudes y distancias fenéticas de Gower.
data DistanceMatrix = DistanceMatrix
  { dmSize           :: !Int
  -- ^ Número de taxones $N$
  , dmSimilarities   :: !(VU.Vector Double)
  -- ^ Matriz $N \times N$ row-major de similitudes $S_{ij} \in [0, 1]$
  , dmDistances      :: !(VU.Vector Double)
  -- ^ Matriz $N \times N$ row-major de distancias $D_{ij} = 1 - S_{ij} \in [0, 1]$
  , dmEuclideanDist  :: !(VU.Vector Double)
  -- ^ Matriz $N \times N$ row-major de distancias euclídeas $D^{\text{E}}_{ij} = \sqrt{1 - S_{ij}}$
  , dmUndefinedPairs :: ![(Int, Int)]
  -- ^ Pares $(i, j)$ con $i < j$ donde $\sum_k w_{ijk} = 0$
  } deriving (Eq, Show, Generic, NFData)

-- | Construye una 'DistanceMatrix' a partir de una función de distancia $D(i, j)$ arbitraria.
-- Útil para algoritmos de agrupamiento y pruebas sobre matrices aditivas o ultramétricas.
fromDistanceFunction :: Int -> (Int -> Int -> Double) -> DistanceMatrix
fromDistanceFunction n distFn
  | n <= 0 = DistanceMatrix 0 VU.empty VU.empty VU.empty []
  | otherwise =
      let dists = VU.generate (n * n) $ \idx ->
            let (i, j) = idx `quotRem` n
            in if i == j then 0.0 else max 0.0 (distFn i j)
          sims = VU.map (\d -> max 0.0 (1.0 - min 1.0 d)) dists
          eucl = VU.map sqrt dists
      in DistanceMatrix n sims dists eucl []

-- | Consulta la similitud $S_{ij}$ entre dos taxones $i$ y $j$ ($0 \le i, j < N$).
lookupSimilarity :: DistanceMatrix -> Int -> Int -> Double
lookupSimilarity dm i j
  | i == j = 1.0
  | i >= 0 && i < n && j >= 0 && j < n = dmSimilarities dm VU.! (i * n + j)
  | otherwise = 0.0
  where
    n = dmSize dm

-- | Consulta la distancia de Gower $D_{ij} = 1 - S_{ij}$ entre dos taxones $i$ y $j$.
lookupDistance :: DistanceMatrix -> Int -> Int -> Double
lookupDistance dm i j
  | i == j = 0.0
  | i >= 0 && i < n && j >= 0 && j < n = dmDistances dm VU.! (i * n + j)
  | otherwise = 1.0
  where
    n = dmSize dm

-- | Consulta la distancia euclídea $D^{\text{E}}_{ij} = \sqrt{1 - S_{ij}}$ entre dos taxones.
lookupEuclideanDistance :: DistanceMatrix -> Int -> Int -> Double
lookupEuclideanDistance dm i j
  | i == j = 0.0
  | i >= 0 && i < n && j >= 0 && j < n = dmEuclideanDist dm VU.! (i * n + j)
  | otherwise = 1.0
  where
    n = dmSize dm

-- | Representación interna precalculada de un carácter para evaluar Gower en $O(1)$ por par.
data GowerCharKernel
  = GowerUnordered !(VU.Vector Word64) !(VU.Vector PseudoTag)
  | GowerOrdered   !Int !(VU.Vector Double) !(VU.Vector PseudoTag)
  | GowerNumeric   !Double !(VU.Vector Double) !(VU.Vector PseudoTag)

-- | Calcula la matriz completa de similitud y distancia de Gower sobre la 'ColumnarMatrix'
-- en tiempo $O(N^2 M)$.
computeGowerMatrix :: SpecsDef -> ColumnarMatrix -> DistanceMatrix
computeGowerMatrix specs mat =
  let !n = cmNumTaxa mat
      kernels = prepareKernels specs mat
      upperPairs =
        [ ((i, j), evalPair kernels i j)
        | i <- [0 .. n - 1]
        , j <- [i + 1 .. n - 1]
        ]
      undefPairs = [ pair | (pair, Nothing) <- upperPairs ]
      pairMap = Map.fromList
        [ (pair, maybe 0.0 id mSim)
        | (pair, mSim) <- upperPairs
        ]
      sims = VU.generate (n * n) $ \idx ->
        let (i, j) = idx `quotRem` n
        in if i == j
             then 1.0
             else
               let key = if i < j then (i, j) else (j, i)
               in Map.findWithDefault 0.0 key pairMap
      dists = VU.generate (n * n) $ \idx ->
        let (i, j) = idx `quotRem` n
        in if i == j
             then 0.0
             else clamp01 (1.0 - (sims VU.! idx))
      eucl = VU.map sqrt dists
  in DistanceMatrix
       { dmSize           = n
       , dmSimilarities   = sims
       , dmDistances      = dists
       , dmEuclideanDist  = eucl
       , dmUndefinedPairs = undefPairs
       }
  where
    clamp01 x = max 0.0 (min 1.0 x)

    evalPair :: [GowerCharKernel] -> Int -> Int -> Maybe Double
    evalPair kernels !i !j =
      let (!sumWS, !sumW) = foldl' (stepKernel i j) (0.0, 0) kernels
      in if sumW == (0 :: Int)
           then Nothing
           else Just (clamp01 (sumWS / fromIntegral sumW))

    stepKernel :: Int -> Int -> (Double, Int) -> GowerCharKernel -> (Double, Int)
    stepKernel !i !j (!accS, !accW) kernel =
      case kernel of
        GowerUnordered col tags ->
          if i < VU.length tags && j < VU.length tags
             && tags VU.! i == tagValid && tags VU.! j == tagValid
            then
              let !bI = col VU.! i
                  !bJ = col VU.! j
                  !u  = popCount (bI .|. bJ)
              in if bI /= 0 && bJ /= 0 && u > 0
                   then
                     let !inter = popCount (bI .&. bJ)
                         !s     = fromIntegral inter / fromIntegral u
                     in (accS + s, accW + 1)
                   else (accS, accW)
            else (accS, accW)

        GowerOrdered m ranks tags ->
          if i < VU.length tags && j < VU.length tags
             && tags VU.! i == tagValid && tags VU.! j == tagValid
            then
              let !rI = ranks VU.! i
                  !rJ = ranks VU.! j
              in if rI > 0.0 && rJ > 0.0
                   then
                     let !s = if m <= 1
                                then 1.0
                                else clamp01 (1.0 - abs (rI - rJ) / fromIntegral (m - 1))
                     in (accS + s, accW + 1)
                   else (accS, accW)
            else (accS, accW)

        GowerNumeric rangeK centers tags ->
          if i < VU.length tags && j < VU.length tags
             && tags VU.! i == tagValid && tags VU.! j == tagValid
            then
              let !xI = centers VU.! i
                  !xJ = centers VU.! j
                  !s  = if rangeK <= 0.0
                          then 1.0
                          else clamp01 (1.0 - abs (xI - xJ) / rangeK)
              in (accS + s, accW + 1)
            else (accS, accW)

-- | Prepara los kernels columnares precalculando rangos medios de Podani y amplitudes $R_k$.
prepareKernels :: SpecsDef -> ColumnarMatrix -> [GowerCharKernel]
prepareKernels specs mat =
  concatMap buildKernel [1 .. cmNumChars mat]
  where
    typeMap = cmCharTypes mat

    buildKernel cid =
      case Map.findWithDefault UM cid typeMap of
        TE  -> []
        UM  -> buildUnordered cid
        EUM -> buildUnordered cid
        OM  -> buildOrdered cid
        EOM -> buildOrdered cid
        IN  -> buildNumeric cid
        RN  -> buildNumeric cid

    buildUnordered cid =
      case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
        (Just col, Just tags) -> [GowerUnordered col tags]
        _                     -> []

    buildOrdered cid =
      case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
        (Just col, Just tags) ->
          let mDecl = Map.findWithDefault (specMaxNumberStates specs) cid (specNumbersOfStates specs)
              n = min (VU.length col) (VU.length tags)
              unionMask = foldl' (\acc i -> if tags VU.! i == tagValid then acc .|. (col VU.! i) else acc) 0 [0 .. n - 1]
              maxObs = maximum (0 : [b + 1 | b <- [0 .. maxBitsetStates - 1], testBit unionMask b])
              !m = max 1 (max mDecl maxObs)
              ranks = VU.generate n $ \i ->
                if tags VU.! i == tagValid
                  then meanStateRank (col VU.! i)
                  else 0.0
          in [GowerOrdered m ranks tags]
        _ -> []

    meanStateRank :: Word64 -> Double
    meanStateRank 0 = 0.0
    meanStateRank msk =
      let states = [fromIntegral (b + 1) :: Double | b <- [0 .. maxBitsetStates - 1], testBit msk b]
      in if null states then 0.0 else sum states / fromIntegral (length states)

    buildNumeric cid =
      case Map.lookup cid (cmNumericCols mat) of
        Nothing -> []
        Just nc ->
          let centers = ncCenter nc
              tags    = ncPseudoTags nc
              n       = min (VU.length centers) (VU.length tags)
              validVals = [centers VU.! i | i <- [0 .. n - 1], tags VU.! i == tagValid]
              !rangeK = case validVals of
                [] -> 0.0
                vs -> maximum vs - minimum vs
          in [GowerNumeric rangeK centers tags]

--------------------------------------------------------------------------------
-- Reconstrucción Fenética: UPGMA (Sokal & Michener, 1958)
--------------------------------------------------------------------------------

data UpgmaCluster = UpgmaCluster
  { ucId     :: !Int
  , ucSize   :: !Int
  , ucHeight :: !Double
  , ucTree   :: !(PhyloTree Int)
  }

-- | Reconstruye un fenograma ultramétrico con longitudes de rama mediante el algoritmo
-- UPGMA (Ecuación 5.13).
buildUPGMA :: DistanceMatrix -> PhyloTree Int
buildUPGMA dm
  | n <= 0    = PhyloLeaf 0 Nothing
  | n == 1    = PhyloLeaf 0 Nothing
  | otherwise =
      let initialClusters = Map.fromList
            [ (i, UpgmaCluster i 1 0.0 (PhyloLeaf i Nothing))
            | i <- [0 .. n - 1]
            ]
          initialDists = Map.fromList
            [ ((i, j), lookupDistance dm i j)
            | i <- [0 .. n - 1]
            , j <- [i + 1 .. n - 1]
            ]
      in stepUPGMA n initialClusters initialDists
  where
    n = dmSize dm

    distLookup :: Map (Int, Int) Double -> Int -> Int -> Double
    distLookup dMap i j
      | i == j    = 0.0
      | i < j     = Map.findWithDefault 1.0 (i, j) dMap
      | otherwise = Map.findWithDefault 1.0 (j, i) dMap

    stepUPGMA
      :: Int
      -> Map Int UpgmaCluster
      -> Map (Int, Int) Double
      -> PhyloTree Int
    stepUPGMA !nextId clusters dMap =
      case Map.elems clusters of
        []  -> PhyloLeaf 0 Nothing
        [c] -> ucTree c
        active ->
          let activeIds = map ucId active
              pairs =
                [ ((i, j), distLookup dMap i j)
                | i <- activeIds
                , j <- activeIds
                , i < j
                ]
              ((bestI, bestJ), minDist) = minimumBy (comparing snd <> comparing fst) pairs
              cA = clusters Map.! bestI
              cB = clusters Map.! bestJ
              !newHeight = minDist / 2.0
              !branchA   = max 0.0 (newHeight - ucHeight cA)
              !branchB   = max 0.0 (newHeight - ucHeight cB)
              newTree = PhyloNode
                (setBranchLength (Just branchA) (ucTree cA))
                (setBranchLength (Just branchB) (ucTree cB))
                Nothing
              !newSize = ucSize cA + ucSize cB
              mergedCluster = UpgmaCluster nextId newSize newHeight newTree
              remainingClusters = Map.delete bestJ (Map.delete bestI clusters)
              otherIds = Map.keys remainingClusters
              !sizeAD = fromIntegral (ucSize cA)
              !sizeBD = fromIntegral (ucSize cB)
              !sizeSum = sizeAD + sizeBD
              newDists = foldl'
                (\m k ->
                  let dAC = distLookup m bestI k
                      dBC = distLookup m bestJ k
                      !dNew = (sizeAD * dAC + sizeBD * dBC) / sizeSum
                  in Map.insert (k, nextId) dNew m
                )
                dMap
                otherIds
              clusters' = Map.insert nextId mergedCluster remainingClusters
          in stepUPGMA (nextId + 1) clusters' newDists

--------------------------------------------------------------------------------
-- Reconstrucción Fenética: Neighbor-Joining (Saitou & Nei, 1987)
--------------------------------------------------------------------------------

-- | Reconstruye un árbol filogenético con longitudes de rama mediante el algoritmo
-- Neighbor-Joining (Ecuaciones 5.14 y 5.15).
buildNeighborJoining :: DistanceMatrix -> PhyloTree Int
buildNeighborJoining dm
  | n <= 0    = PhyloLeaf 0 Nothing
  | n == 1    = PhyloLeaf 0 Nothing
  | otherwise =
      let initialClusters = Map.fromList
            [ (i, PhyloLeaf i Nothing)
            | i <- [0 .. n - 1]
            ]
          initialDists = Map.fromList
            [ ((i, j), lookupDistance dm i j)
            | i <- [0 .. n - 1]
            , j <- [i + 1 .. n - 1]
            ]
      in stepNJ n initialClusters initialDists
  where
    n = dmSize dm

    distLookup :: Map (Int, Int) Double -> Int -> Int -> Double
    distLookup dMap i j
      | i == j    = 0.0
      | i < j     = Map.findWithDefault 1.0 (i, j) dMap
      | otherwise = Map.findWithDefault 1.0 (j, i) dMap

    stepNJ
      :: Int
      -> Map Int (PhyloTree Int)
      -> Map (Int, Int) Double
      -> PhyloTree Int
    stepNJ !nextId clusters dMap =
      case Map.toAscList clusters of
        [] -> PhyloLeaf 0 Nothing
        [(_, t)] -> t
        [(i, tI), (j, tJ)] ->
          let !dIJ = distLookup dMap i j
              !half = max 0.0 (dIJ / 2.0)
          in PhyloNode
               (setBranchLength (Just half) tI)
               (setBranchLength (Just half) tJ)
               Nothing
        activePairs ->
          let activeIds = map fst activePairs
              !r = length activeIds
              !rMinus2 = fromIntegral (r - 2) :: Double
              rowSums = Map.fromList
                [ (i, sum [distLookup dMap i k | k <- activeIds, k /= i])
                | i <- activeIds
                ]
              qCandidates =
                [ let !distPair = distLookup dMap i j
                      !sumI     = Map.findWithDefault 0.0 i rowSums
                      !sumJ     = Map.findWithDefault 0.0 j rowSums
                      !qIJ      = rMinus2 * distPair - sumI - sumJ
                      -- Cuantizar Q_ij a 1e-12 para desempatar por menor distancia D_ij
                      !qKey     = fromIntegral (round (qIJ * 1.0e12) :: Integer) :: Double
                  in ((qKey, distPair, i, j), (i, j, distPair, sumI, sumJ))
                | i <- activeIds
                , j <- activeIds
                , i < j
                ]
              (_, (bestI, bestJ, dIJ, rI, rJ)) = minimumBy (comparing fst) qCandidates
              !deltaIU = max 0.0 (0.5 * dIJ + (rI - rJ) / (2.0 * rMinus2))
              !deltaJU = max 0.0 (dIJ - deltaIU)
              tI = clusters Map.! bestI
              tJ = clusters Map.! bestJ
              newTree = PhyloNode
                (setBranchLength (Just deltaIU) tI)
                (setBranchLength (Just deltaJU) tJ)
                Nothing
              remainingClusters = Map.delete bestJ (Map.delete bestI clusters)
              otherIds = Map.keys remainingClusters
              newDists = foldl'
                (\m k ->
                  let dIK = distLookup m bestI k
                      dJK = distLookup m bestJ k
                      !dUK = max 0.0 (0.5 * (dIK + dJK - dIJ))
                  in Map.insert (k, nextId) dUK m
                )
                dMap
                otherIds
              clusters' = Map.insert nextId newTree remainingClusters
          in stepNJ (nextId + 1) clusters' newDists
