{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Core.Item
Description : Definición de taxones e ítems descriptivos en el formato ITEMS de DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Core.Item
  ( ItemType (..)
  , ItemDef (..)
  , mkPrimaryItem
  , mkVariantItem
  , lookupAttribute
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

import Taxide.Core.Types (RawAttribute)

-- | Distinción entre taxones principales ('#') e ítems variantes ('#+').
-- Un ítem variante (#+) hereda todos los atributos de su progenitor primario
-- salvo aquellos redefinidos explícitamente.
data ItemType
  = PrimaryItem -- ^ Taxón primario estándar: @# Taxon_name/@
  | VariantItem -- ^ Ítem variante subordinado: @#+ Taxon_variant/@
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Definición de un taxón/ítem en el archivo ITEMS.
data ItemDef = ItemDef
  { itemType       :: !ItemType               -- ^ '#' vs '#+'
  , itemName       :: !Text                   -- ^ Nombre formal del taxón, ej. "Craspedacusta sowerbyi"
  , itemAuthority  :: !(Maybe Text)           -- ^ Autoridad taxonómica extraída de comentarios angulares <...>, ej. "Lankester, 1880"
  , itemAttributes :: ![(Int, RawAttribute)]  -- ^ Lista de pares (NúmeroCarácter, Atributo)
  , itemComments   :: ![Text]                 -- ^ Comentarios de encabezado o notas taxonómicas
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Constructor para un ítem primario.
mkPrimaryItem :: Text -> [(Int, RawAttribute)] -> ItemDef
mkPrimaryItem name attrs = ItemDef
  { itemType       = PrimaryItem
  , itemName       = name
  , itemAuthority  = Nothing
  , itemAttributes = attrs
  , itemComments   = []
  }

-- | Constructor para un ítem variante ('#+').
mkVariantItem :: Text -> [(Int, RawAttribute)] -> ItemDef
mkVariantItem name attrs = ItemDef
  { itemType       = VariantItem
  , itemName       = name
  , itemAuthority  = Nothing
  , itemAttributes = attrs
  , itemComments   = []
  }

-- | Busca el atributo correspondiente a un carácter dado en el ítem.
lookupAttribute :: Int -> ItemDef -> Maybe RawAttribute
lookupAttribute charNum item = lookup charNum (itemAttributes item)
