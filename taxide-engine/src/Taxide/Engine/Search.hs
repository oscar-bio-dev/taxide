{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Engine.Search
Description : Búsqueda heurística de árboles de máxima parsimonia (Wagner, SPR, TBR) en paralelo
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Implementa los algoritmos de búsqueda topológica especificados en la Sección 5.5:
1. Construcción determinista por Adición Secuencial de Wagner ('wagnerBuild').
2. Búsqueda heurística de subida de colina (*hill-climbing*) en vecindades SPR ('searchSPR')
   y TBR ('searchTBR'), paralelizada de forma determinista por lotes mediante
   @withStrategy (parListChunk 64 rdeepseq)@.
-}

module Taxide.Engine.Search
  ( SearchStrategy (..)
  , SearchResult (..)
  , wagnerBuild
  , searchSPR
  , searchTBR
  , runParsimonySearch
  ) where

import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (parListChunk, rdeepseq, withStrategy)
import GHC.Generics (Generic)

import Taxide.Core.Specs (SpecsDef)
import Taxide.Engine.Discretize (DiscretizeMethod, DiscretizeTarget)
import Taxide.Engine.Matrix (ColumnarMatrix (..))
import Taxide.Engine.Sankoff
  ( CompiledParsimonyMatrix
  , TreeEvaluation (..)
  , compileParsimonyMatrix
  , evaluateCompiledTree
  )
import Taxide.Engine.Tree
  ( PhyloTree (..)
  , clearBranchLengths
  , insertLeafEverywhere
  , sprNeighbors
  , tbrNeighbors
  )
import Taxide.Engine.Variants (ResolvedTaxon)

-- | Estrategia de búsqueda topológica para máxima parsimonia.
data SearchStrategy
  = SearchWagner
  -- ^ Sólo construcción por adición secuencial de Wagner
  | SearchSPR
  -- ^ Wagner + refinamiento por Subtree Pruning and Regrafting (SPR)
  | SearchTBR
  -- ^ Wagner + refinamiento por Tree Bisection and Reconnection (TBR)
  deriving (Eq, Ord, Show, Read, Generic, NFData)

-- | Resultado de una búsqueda heurística de máxima parsimonia.
data SearchResult = SearchResult
  { srBestTree   :: !(PhyloTree Int) -- ^ Árbol óptimo hallado (indexado 0..N-1)
  , srEvaluation :: !TreeEvaluation  -- ^ Métricas cladísticas ($L, CI, RI, RC, HI$)
  , srIterations :: !Int             -- ^ Número de pasos de mejora topológica ejecutados
  , srCandidates :: !Int             -- ^ Total de topologías candidatas evaluadas
  } deriving (Eq, Show, Generic, NFData)

-- | Evalúa en paralelo determinista una lista de árboles candidatos por lotes de 64.
evaluateBatchParallel
  :: CompiledParsimonyMatrix
  -> [PhyloTree Int]
  -> [(PhyloTree Int, TreeEvaluation)]
evaluateBatchParallel compiled candidates =
  withStrategy (parListChunk 64 rdeepseq)
    [ (t, evaluateCompiledTree compiled t) | t <- candidates ]

-- | Selecciona de manera determinista el árbol de longitud mínima de una lista no vacía.
selectBestCandidate
  :: (PhyloTree Int, TreeEvaluation)
  -> [(PhyloTree Int, TreeEvaluation)]
  -> (PhyloTree Int, TreeEvaluation)
selectBestCandidate = foldl' pickBetter
  where
    pickBetter best@(_, evalBest) cand@(_, evalCand) =
      if teTotalLength evalCand < teTotalLength evalBest - 1.0e-9
        then cand
        else best

-- | Construcción determinista del árbol inicial por adición secuencial de Wagner.
--
-- Parte del par $(0, 1)$ e inserta secuencialmente cada taxón $t \in \{2, \dots, N-1\}$
-- en la arista que minimiza la longitud total de parsimonia $L(T)$.
wagnerBuild
  :: DiscretizeMethod
  -> DiscretizeTarget
  -> SpecsDef
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> PhyloTree Int
wagnerBuild method target specs taxa mat =
  let compiled = compileParsimonyMatrix method target specs taxa mat
  in fst (wagnerBuildCompiled compiled (cmNumTaxa mat))

-- | Versión interna de 'wagnerBuild' que reutiliza la matriz compilada y contabiliza candidatos.
wagnerBuildCompiled
  :: CompiledParsimonyMatrix
  -> Int
  -> (PhyloTree Int, Int)
wagnerBuildCompiled compiled nTaxa
  | nTaxa <= 1 = (PhyloLeaf 0 Nothing, 1)
  | nTaxa == 2 = (PhyloNode (PhyloLeaf 0 Nothing) (PhyloLeaf 1 Nothing) Nothing, 1)
  | otherwise  =
      let initialTree = PhyloNode (PhyloLeaf 0 Nothing) (PhyloLeaf 1 Nothing) Nothing
      in foldl' addNextTaxon (initialTree, 1) [2 .. nTaxa - 1]
  where
    addNextTaxon (!currTree, !accCount) !nextTaxon =
      let candidates = insertLeafEverywhere nextTaxon currTree
          evaluated  = evaluateBatchParallel compiled candidates
      in case evaluated of
           []       -> (currTree, accCount)
           (x : xs) ->
             let (bestTree, _) = selectBestCandidate x xs
             in (bestTree, accCount + length candidates)

-- | Búsqueda heurística de subida de colina (*hill-climbing*) explorando la vecindad
-- SPR (*Subtree Pruning and Regrafting*) hasta alcanzar un óptimo local.
searchSPR
  :: DiscretizeMethod
  -> DiscretizeTarget
  -> SpecsDef
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> PhyloTree Int
  -> SearchResult
searchSPR method target specs taxa mat initialTree =
  let compiled = compileParsimonyMatrix method target specs taxa mat
  in hillClimb sprNeighbors compiled initialTree

-- | Búsqueda heurística de subida de colina (*hill-climbing*) explorando la vecindad
-- TBR (*Tree Bisection and Reconnection*) hasta alcanzar un óptimo local.
searchTBR
  :: DiscretizeMethod
  -> DiscretizeTarget
  -> SpecsDef
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> PhyloTree Int
  -> SearchResult
searchTBR method target specs taxa mat initialTree =
  let compiled = compileParsimonyMatrix method target specs taxa mat
  in hillClimb tbrNeighbors compiled initialTree

-- | Bucle general de escalada de colina parametrizado por el generador de vecindad.
hillClimb
  :: (PhyloTree Int -> [PhyloTree Int])
  -> CompiledParsimonyMatrix
  -> PhyloTree Int
  -> SearchResult
hillClimb neighborFn compiled startTree =
  let cleanStart = clearBranchLengths startTree
      startEval  = evaluateCompiledTree compiled cleanStart
  in go cleanStart startEval 0 1
  where
    go !currTree !currEval !iter !totalCands =
      let neighbors = neighborFn currTree
      in if null neighbors
           then SearchResult currTree currEval iter totalCands
           else
             let evaluated = evaluateBatchParallel compiled neighbors
                 !newTotal = totalCands + length neighbors
             in case evaluated of
                  [] -> SearchResult currTree currEval iter newTotal
                  (x : xs) ->
                    let (bestNeighbor, bestEval) = selectBestCandidate x xs
                    in if teTotalLength bestEval < teTotalLength currEval - 1.0e-9
                         then go bestNeighbor bestEval (iter + 1) newTotal
                         else SearchResult currTree currEval iter newTotal

-- | Ejecuta el pipeline completo de búsqueda de máxima parsimonia según la estrategia
-- seleccionada (Wagner, SPR o TBR).
runParsimonySearch
  :: SearchStrategy
  -> DiscretizeMethod
  -> DiscretizeTarget
  -> SpecsDef
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> SearchResult
runParsimonySearch strategy method target specs taxa mat =
  let compiled = compileParsimonyMatrix method target specs taxa mat
      (wTree, wCands) = wagnerBuildCompiled compiled (cmNumTaxa mat)
  in case strategy of
       SearchWagner ->
         let eval = evaluateCompiledTree compiled wTree
         in SearchResult wTree eval 0 wCands
       SearchSPR ->
         let res = hillClimb sprNeighbors compiled wTree
         in res { srCandidates = srCandidates res + wCands - 1 }
       SearchTBR ->
         let res = hillClimb tbrNeighbors compiled wTree
         in res { srCandidates = srCandidates res + wCands - 1 }
