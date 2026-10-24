{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Core.Invariants
Description : Validación de invariantes biológicas y lógicas del estándar DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Core.Invariants
  ( isDependencyTriggered
  , validateExclusivePolymorphism
  , validateStatRange
  , validateStateBounds
  , collectActiveStates
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Taxide.Core.Types
  ( CharacterType (..)
  , RawStateExpr (..)
  , StateId
  , StateToken (..)
  , StatRange (..)
  )

-- | Determina si una regla de caracteres dependientes (*DEPENDENT CHARACTERS) se dispara.
--
-- Regla Biológica de TAXIDE:
-- Un carácter dependiente se fuerza a Inapplicable ('-') si y solo si el conjunto
-- de estados observados en el controlador (S_obs) es un subconjunto no vacío del
-- conjunto inhabilitante (S_inh), es decir: S_obs ⊆ S_inh.
--
-- Si existe incertidumbre o polimorfismo que incluye al menos un estado habilitante
-- (ej. S_obs = {1, 2} donde solo {1} inhabilita), la regla NO se dispara obligatoriamente,
-- permitiendo descripciones biológicas válidas como @10,1/2 12,1/-@.
isDependencyTriggered
  :: Set StateId -- ^ S_obs: Estados observados en el carácter controlador
  -> Set StateId -- ^ S_inh: Conjunto de estados inhabilitantes declarados en *DEPENDENT CHARS
  -> Bool
isDependencyTriggered obsStates disablingStates =
  not (Set.null obsStates) && obsStates `Set.isSubsetOf` disablingStates

-- | Valida que un carácter de tipo exclusivo (EUM o EOM) no contenga el operador
-- de polimorfismo simultáneo '&' (StateAnd).
validateExclusivePolymorphism :: CharacterType -> RawStateExpr -> Bool
validateExclusivePolymorphism ctype expr = case ctype of
  EUM -> not (hasStateAnd expr)
  EOM -> not (hasStateAnd expr)
  _   -> True
  where
    hasStateAnd :: RawStateExpr -> Bool
    hasStateAnd (StateAnd _ _)   = True
    hasStateAnd (StateOr a b)    = hasStateAnd a || hasStateAnd b
    hasStateAnd (SingleState _)  = False
    hasStateAnd (RangeTo _ _)    = False
    hasStateAnd (PseudoState {}) = False

-- | Valida la coherencia métrica de un rango cuantitativo: min <= normalMin <= normalMax <= max.
validateStatRange :: Ord a => StatRange a -> Bool
validateStatRange sr =
  let nMin = srNormalMin sr
      nMax = srNormalMax sr
      normOk = nMin <= nMax
      minOk = case srAbsoluteMin sr of
        Just aMin -> aMin <= nMin
        Nothing   -> True
      maxOk = case srAbsoluteMax sr of
        Just aMax -> nMax <= aMax
        Nothing   -> True
      centerOk = case srCenterVal sr of
        Just c  -> nMin <= c && c <= nMax
        Nothing -> True
  in normOk && minOk && maxOk && centerOk

-- | Valida que los identificadores de estado pertenezcan al intervalo [1, maxStates].
validateStateBounds :: Int -> RawStateExpr -> Bool
validateStateBounds maxStates expr =
  let states = collectActiveStates expr
  in all (\s -> s >= 1 && fromIntegral s <= maxStates) states

-- | Extrae todos los identificadores de estados individuales presentes en un 'RawStateExpr'.
collectActiveStates :: RawStateExpr -> Set StateId
collectActiveStates expr = case expr of
  SingleState st      -> Set.singleton (stId st)
  RangeTo st1 st2     -> Set.fromList [stId st1 .. stId st2]
  StateOr e1 e2       -> collectActiveStates e1 `Set.union` collectActiveStates e2
  StateAnd e1 e2      -> collectActiveStates e1 `Set.union` collectActiveStates e2
  PseudoState {}      -> Set.empty
