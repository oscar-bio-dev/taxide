{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Engine.Variants
Description : Resolución secuencial y herencia monoidal de ítems variantes (#+)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Engine.Variants
  ( AttrOrigin (..)
  , AttributedState (..)
  , ResolvedTaxon (..)
  , resolveVariants
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

import Taxide.Core.Item (ItemDef (..), ItemType (..))
import Taxide.Core.Types (RawAttribute, StateId)
import Taxide.Engine.Diagnostic
  ( EngineError (DuplicateCharacterError, OrphanVariantError)
  , LintDiagnostic
  , engineErrorToDiagnostic
  )

-- | Origen y trazabilidad de un atributo taxonómico en el motor semántico.
data AttrOrigin
  = OriginExplicit
  -- ^ Declarado explícitamente en la descripción del taxón
  | OriginInherited !Text
  -- ^ Heredado del taxón progenitor primario (#) indicado por su nombre
  | OriginImplicitOmitted !StateId
  -- ^ Inyectado por la directiva *IMPLICIT VALUES al estar omitido
  | OriginImplicitUnspecified !StateId
  -- ^ Inyectado por la directiva *IMPLICIT VALUES al declararse sin estado
  | OriginInapplicableDependency !Int
  -- ^ Forzado a Inapplicable por la regla del DAG del carácter controlador
  deriving (Eq, Show, Read, Generic, NFData, Hashable)

-- | Atributo intermedio con su valor crudo y trazabilidad biológica.
data AttributedState = AttributedState
  { asRaw    :: !RawAttribute
  , asOrigin :: !AttrOrigin
  } deriving (Eq, Show, Read, Generic, NFData, Hashable)

-- | Taxón evaluado en el motor semántico con sus atributos mapeados.
data ResolvedTaxon = ResolvedTaxon
  { rtIndex      :: !Int
  -- ^ Posición ordinal del taxón en el dataset (1..N)
  , rtType       :: !ItemType
  -- ^ PrimaryItem (#) o VariantItem (#+)
  , rtName       :: !Text
  -- ^ Nombre formal del taxón
  , rtAuthority  :: !(Maybe Text)
  -- ^ Autoridad taxonómica (autor, año)
  , rtAttributes :: !(Map Int AttributedState)
  -- ^ Mapa de atributos (NúmeroCarácter -> Atributo)
  , rtComments   :: ![Text]
  -- ^ Comentarios asociados
  } deriving (Eq, Show, Read, Generic, NFData, Hashable)

-- | Resuelve la herencia de variantes (#+) a lo largo de la secuencia de ítems.
--
-- Conforme a la especificación DELTA:
-- 1. Un ítem primario (#) actualiza el contexto del progenitor activo.
-- 2. Un ítem variante (#+) hereda todos los atributos de su progenitor primario,
--    aplicando unión monoidal sesgada a la izquierda (los atributos explícitos
--    del variante sobrescriben los heredados).
-- 3. Si un archivo ITEMS comienza con un variante (#+) huérfano, emite 'OrphanVariantError'.
resolveVariants :: [ItemDef] -> ([ResolvedTaxon], [LintDiagnostic])
resolveVariants items = go 1 Nothing items [] []
  where
    go _ _ [] accTaxa accDiags =
      (reverse accTaxa, reverse accDiags)

    go !idx mLastPrimary (item : rest) accTaxa accDiags0 =
      let accDiags = reverse (duplicateCharDiags item) ++ accDiags0
      in case itemType item of
        PrimaryItem ->
          let explicitAttrs = Map.fromList (itemAttributes item)
              resolvedMap = Map.map (`AttributedState` OriginExplicit) explicitAttrs
              taxon = ResolvedTaxon
                { rtIndex      = idx
                , rtType       = PrimaryItem
                , rtName       = itemName item
                , rtAuthority  = itemAuthority item
                , rtAttributes = resolvedMap
                , rtComments   = itemComments item
                }
              newParent = Just (itemName item, explicitAttrs)
          in go (idx + 1) newParent rest (taxon : accTaxa) accDiags

        VariantItem ->
          case mLastPrimary of
            Nothing ->
              let explicitAttrs = Map.fromList (itemAttributes item)
                  resolvedMap = Map.map (`AttributedState` OriginExplicit) explicitAttrs
                  err = engineErrorToDiagnostic (OrphanVariantError (itemName item))
                  taxon = ResolvedTaxon
                    { rtIndex      = idx
                    , rtType       = VariantItem
                    , rtName       = itemName item
                    , rtAuthority  = itemAuthority item
                    , rtAttributes = resolvedMap
                    , rtComments   = itemComments item
                    }
              in go (idx + 1) Nothing rest (taxon : accTaxa) (err : accDiags)

            Just (parentName, parentAttrs) ->
              let explicitAttrs = Map.fromList (itemAttributes item)
                  explicitTagged = Map.map (`AttributedState` OriginExplicit) explicitAttrs
                  inheritedTagged = Map.map (`AttributedState` OriginInherited parentName) parentAttrs
                  -- Unión monoidal sesgada a la izquierda: los atributos del variante tienen prioridad
                  combinedAttrs = Map.union explicitTagged inheritedTagged
                  taxon = ResolvedTaxon
                    { rtIndex      = idx
                    , rtType       = VariantItem
                    , rtName       = itemName item
                    , rtAuthority  = itemAuthority item
                    , rtAttributes = combinedAttrs
                    , rtComments   = itemComments item
                    }
              in go (idx + 1) mLastPrimary rest (taxon : accTaxa) accDiags

    -- Un carácter repetido dentro de un ítem descarta silenciosamente todos los
    -- valores salvo el último ('Map.fromList'); se reporta como TAX-DATA-003.
    duplicateCharDiags item =
      [ engineErrorToDiagnostic (DuplicateCharacterError (itemName item) cid n)
      | (cid, n) <- Map.toList (Map.fromListWith (+) [(c, 1 :: Int) | (c, _) <- itemAttributes item])
      , n > 1
      ]
