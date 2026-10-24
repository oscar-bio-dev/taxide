{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Engine.Implicits
Description : Inyección determinista de valores por defecto (*IMPLICIT VALUES)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Engine.Implicits
  ( injectImplicits
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Taxide.Core.Specs (ImplicitRule (..), SpecsDef (..))
import Taxide.Core.Types
  ( CharacterType (..)
  , RawAttribute (..)
  , RawStateExpr (..)
  , StateToken (..)
  )
import Taxide.Engine.Diagnostic
  ( EngineError (InvalidImplicitTargetType)
  , LintDiagnostic
  , engineErrorToDiagnostic
  )
import Taxide.Engine.Variants (AttrOrigin (..), AttributedState (..), ResolvedTaxon (..))

-- | Inyecta estados implícitos (*IMPLICIT VALUES) en cada taxón resuelto.
--
-- Restricciones y Orden Biológico:
-- 1. Se ejecuta OBLIGATORIAMENTE después de la herencia de variantes (para no
--    sobrescribir atributos ancestrales que un '#+' debe heredar de su '#').
-- 2. Se ejecuta ANTES de la propagación del DAG de dependencias.
-- 3. Si una regla se define sobre un carácter no cualitativo (IN, RN, TE),
--    se emite 'InvalidImplicitTargetType' (TAX-IMP-001).
-- 4. Para cada carácter cualitativo con regla (c, s:t):
--    - Si 'c' está omitido en el taxón, se inyecta el estado 's' marcado con
--      'OriginImplicitOmitted s'.
--    - Si 'c' fue puntuado sin estado explícito, se inyecta 't'.
injectImplicits :: SpecsDef -> [ResolvedTaxon] -> ([ResolvedTaxon], [LintDiagnostic])
injectImplicits specs taxa =
  let (validRules, typeDiagnostics) = validateImplicitRules (specCharTypes specs) (specImplicitValues specs)
      updatedTaxa = map (applyImplicitRules validRules) taxa
  in (updatedTaxa, typeDiagnostics)

-- | Valida que las directivas *IMPLICIT VALUES solo apunten a caracteres cualitativos.
validateImplicitRules
  :: Map Int CharacterType
  -> [ImplicitRule]
  -> ([ImplicitRule], [LintDiagnostic])
validateImplicitRules typeMap rules = foldr check ([], []) rules
  where
    check rule (accRules, accDiags) =
      let cid = impCharId rule
          mType = Map.lookup cid typeMap
      in case mType of
        Just IN ->
          (accRules, engineErrorToDiagnostic (InvalidImplicitTargetType cid IN) : accDiags)
        Just RN ->
          (accRules, engineErrorToDiagnostic (InvalidImplicitTargetType cid RN) : accDiags)
        Just TE ->
          (accRules, engineErrorToDiagnostic (InvalidImplicitTargetType cid TE) : accDiags)
        _ ->
          (rule : accRules, accDiags)

-- | Aplica las reglas válidas sobre un taxón individual.
applyImplicitRules :: [ImplicitRule] -> ResolvedTaxon -> ResolvedTaxon
applyImplicitRules rules taxon =
  let initialAttrs = rtAttributes taxon
      finalAttrs = foldl applyRule initialAttrs rules
  in taxon { rtAttributes = finalAttrs }
  where
    applyRule attrs rule =
      let cid = impCharId rule
          sOmitted = impOmitted rule
          mUnspec = impUnspecified rule
      in case Map.lookup cid attrs of
        Nothing ->
          -- Carácter omitido completamente: inyectar estado 's'
          let injectedAttr = AttributedState
                { asRaw    = AttrStates Nothing (SingleState (StateToken sOmitted Nothing))
                , asOrigin = OriginImplicitOmitted sOmitted
                }
          in Map.insert cid injectedAttr attrs

        Just (AttributedState raw _orig) ->
          case mUnspec of
            Just t | isUnspecified raw ->
              -- Carácter declarado sin estado explícito: inyectar estado 't'
              let injectedAttr = AttributedState
                    { asRaw    = AttrStates Nothing (SingleState (StateToken t Nothing))
                    , asOrigin = OriginImplicitUnspecified t
                    }
              in Map.insert cid injectedAttr attrs
            _ ->
              -- Ya tiene un valor activo o no requiere sustitución
              attrs

    isUnspecified (AttrStates _ (SingleState (StateToken 0 _))) = True
    isUnspecified _                                            = False
