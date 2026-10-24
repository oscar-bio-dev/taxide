{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Engine.Dependencies
Description : Compilación, verificación topológica y propagación reactiva del DAG de dependencias
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Engine.Dependencies
  ( checkDependencyDAG
  , propagateDependencies
  , topologicalSortDAG
  ) where

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Taxide.Core.Invariants (collectActiveStates, isDependencyTriggered)
import Taxide.Core.Specs (DependentRule (..), SpecsDef (..))
import Taxide.Core.Types
  ( PseudoValue (Inapplicable)
  , RawAttribute (..)
  , RawStateExpr (..)
  )
import Taxide.Engine.Diagnostic
  ( EngineError (..)
  , LintDiagnostic (..)
  , engineErrorToDiagnostic
  )
import Taxide.Engine.Variants
  ( AttrOrigin (..)
  , AttributedState (..)
  , ResolvedTaxon (..)
  )

-- | Verifica estáticamente que las reglas *DEPENDENT CHARACTERS formen un Grafo
-- Acíclico Dirigido (DAG).
--
-- Si existe un ciclo dirigido entre caracteres (ej. A -> B -> A), devuelve
-- 'Left (CyclicDependencyError ciclo)'. Si es acíclico, devuelve 'Right ordenTopologico'.
checkDependencyDAG :: [DependentRule] -> Either EngineError [Int]
checkDependencyDAG rules =
  let graph = buildAdjacencyMap rules
      allNodes = Map.keysSet graph
  in case topologicalSortDAG graph of
       Just sortedOrder -> Right sortedOrder
       Nothing          -> Left (CyclicDependencyError (findCycle graph (Set.toList allNodes)))

-- | Construye el mapa de adyacencia de dependencias: Controlador -> [Dependientes].
buildAdjacencyMap :: [DependentRule] -> Map Int [Int]
buildAdjacencyMap rules = foldl addRule Map.empty rules
  where
    addRule acc (DependentRule c _ deps) =
      let currentDeps = Map.findWithDefault [] c acc
          accWithCtrl = Map.insert c (currentDeps ++ deps) acc
          -- Asegurar que los nodos destino también existan en el mapa
          accWithAll = foldl (\m d -> if Map.member d m then m else Map.insert d [] m) accWithCtrl deps
      in accWithAll

-- | Ordenamiento topológico utilizando el Algoritmo de Kahn.
-- Devuelve 'Just [nodo]' si el grafo es un DAG, o 'Nothing' si contiene ciclos.
topologicalSortDAG :: Map Int [Int] -> Maybe [Int]
topologicalSortDAG graph = go initialQueue initialInDegrees []
  where
    allNodes = Map.keys graph

    -- Calcular in-degree de cada nodo
    initialInDegrees :: Map Int Int
    initialInDegrees = foldl countIn (Map.fromSet (const 0) (Map.keysSet graph)) allNodes
      where
        countIn degMap u =
          let neighbors = Map.findWithDefault [] u graph
          in foldl (\m v -> Map.adjust (+ 1) v m) degMap neighbors

    -- Nodos con in-degree = 0
    initialQueue :: [Int]
    initialQueue = [u | (u, deg) <- Map.toList initialInDegrees, deg == 0]

    go [] _degMap acc =
      if length acc == Map.size graph
        then Just (reverse acc)
        else Nothing

    go (u : q) degMap acc =
      let neighbors = Map.findWithDefault [] u graph
          (newQ, newDeg) = foldl decDegree (q, degMap) neighbors
      in go newQ newDeg (u : acc)

    decDegree (q, degs) v =
      let currentDeg = Map.findWithDefault 1 v degs
          updatedDeg = currentDeg - 1
          degs' = Map.insert v updatedDeg degs
      in if updatedDeg == 0
           then (q ++ [v], degs')
           else (q, degs')

-- | Localiza un ciclo dirigido mediante DFS para reportar diagnósticos claros.
findCycle :: Map Int [Int] -> [Int] -> [Int]
findCycle graph nodes =
  case find (not . null) (map (dfsCycle []) nodes) of
    Just c  -> c
    Nothing -> take 3 nodes -- Fallback si el ciclo es complejo
  where
    dfsCycle visited u
      | u `elem` visited = dropWhile (/= u) (reverse (u : visited))
      | otherwise =
          let neighbors = Map.findWithDefault [] u graph
          in case find (not . null) (map (dfsCycle (u : visited)) neighbors) of
               Just cycleFound -> cycleFound
               Nothing         -> []

-- | Evalúa y propaga las reglas de inhabilitación en orden topológico para cada taxón.
propagateDependencies
  :: SpecsDef
  -> [ResolvedTaxon]
  -> ([ResolvedTaxon], [LintDiagnostic])
propagateDependencies specs taxa =
  let rules = specDependencies specs
  in case checkDependencyDAG rules of
       Left err ->
         -- Si hay un ciclo crítico, se emite el error y se retorna sin propagar
         (taxa, [engineErrorToDiagnostic err])

       Right _sortedChars ->
         -- Grafo acíclico válido: propagar reglas sobre cada taxón
         let results = map (propagateTaxonRules rules) taxa
             updatedTaxa = map fst results
             allDiags = concatMap snd results
         in (updatedTaxa, allDiags)

-- | Aplica todas las reglas de dependencias a un taxón individual.
propagateTaxonRules
  :: [DependentRule]
  -> ResolvedTaxon
  -> (ResolvedTaxon, [LintDiagnostic])
propagateTaxonRules rules taxon =
  foldl evaluateRule (taxon, []) rules
  where
    taxonName = rtName taxon

    evaluateRule (currTaxon, accDiags) (DependentRule ctrlId disStates deps) =
      let currentAttrs = rtAttributes currTaxon
          obsStates = getObservedStates ctrlId currentAttrs
          triggered = isDependencyTriggered obsStates disStates
      in if triggered
           then applyInactivation ctrlId deps currTaxon accDiags
           else (currTaxon, accDiags)

    -- Extrae los estados biológicos activos del carácter controlador
    getObservedStates cid attrs =
      case Map.lookup cid attrs of
        Nothing -> Set.empty
        Just (AttributedState raw _) ->
          case raw of
            AttrStates _ expr -> collectActiveStates expr
            _                 -> Set.empty

    -- Forzar a Inapplicable a los caracteres dependientes
    applyInactivation ctrlId deps currTaxon accDiags =
      foldl (inactivateDep ctrlId) (currTaxon, accDiags) deps

    inactivateDep ctrlId (currTaxon, diags) depId =
      let currentAttrs = rtAttributes currTaxon
          inappAttr = AttributedState
            { asRaw    = AttrStates Nothing (PseudoState Inapplicable Nothing)
            , asOrigin = OriginInapplicableDependency ctrlId
            }
      in case Map.lookup depId currentAttrs of
        Nothing ->
          -- Carácter dependiente omitido: se fija a Inapplicable sin error
          let newAttrs = Map.insert depId inappAttr currentAttrs
          in (currTaxon { rtAttributes = newAttrs }, diags)

        Just (AttributedState raw orig) ->
          case orig of
            OriginImplicitOmitted _ ->
              -- Era un valor asignado por defecto implícito: se sobrescribe a Inapplicable
              let newAttrs = Map.insert depId inappAttr currentAttrs
              in (currTaxon { rtAttributes = newAttrs }, diags)

            OriginImplicitUnspecified _ ->
              let newAttrs = Map.insert depId inappAttr currentAttrs
              in (currTaxon { rtAttributes = newAttrs }, diags)

            _ ->
              -- Declarado explícitamente o heredado
              case raw of
                AttrStates _ (PseudoState Inapplicable _) ->
                  -- Ya estaba marcado explícitamente como '-': coherente
                  let newAttrs = Map.insert depId inappAttr currentAttrs
                  in (currTaxon { rtAttributes = newAttrs }, diags)

                _ ->
                  -- Contradicción biológica: tiene un estado activo a pesar de estar inhabilitado
                  let errDiag = engineErrorToDiagnostic
                                  (ContradictoryDependentStateError taxonName ctrlId depId)
                      newAttrs = Map.insert depId inappAttr currentAttrs
                  in (currTaxon { rtAttributes = newAttrs }, errDiag : diags)
