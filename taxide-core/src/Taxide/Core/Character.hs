{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Core.Character
Description : Definición de caracteres biológicos según directivas CHARS de DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Core.Character
  ( CharacterDef (..)
  , mkMultistateChar
  , mkNumericChar
  , mkTextChar
  , isMultistate
  , isNumeric
  , isExclusive
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

import Taxide.Core.Types (CharacterType (..), StateId)

-- | Definición de un carácter biológico según el archivo CHARS.
data CharacterDef = CharacterDef
  { charNumber   :: !Int               -- ^ Número secuencial del carácter (1..N)
  , charType     :: !CharacterType     -- ^ Tipo de carácter (UM, OM, IN, RN, etc.)
  , charHeading  :: !(Maybe Text)      -- ^ Encabezado o grupo taxonómico opcional: <features of leaves>
  , charFeature  :: !Text              -- ^ Nombre de la característica biológica (ej. "leaves")
  , charUnits    :: !(Maybe Text)      -- ^ Unidades dimensionales para RN/IN (ej. "cm", "mm")
  , charStates   :: ![(StateId, Text)] -- ^ Lista de estados discretos (1, "simple")/ (2, "compound")
  , charComments :: ![Text]            -- ^ Comentarios adicionales asociados al carácter
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Constructor auxiliar para caracteres cualitativos multiestado (UM/OM).
mkMultistateChar :: Int -> CharacterType -> Text -> [(StateId, Text)] -> CharacterDef
mkMultistateChar num ctype feat states = CharacterDef
  { charNumber   = num
  , charType     = ctype
  , charHeading  = Nothing
  , charFeature  = feat
  , charUnits    = Nothing
  , charStates   = states
  , charComments = []
  }

-- | Constructor auxiliar para caracteres cuantitativos (IN/RN).
mkNumericChar :: Int -> CharacterType -> Text -> Maybe Text -> CharacterDef
mkNumericChar num ctype feat mUnits = CharacterDef
  { charNumber   = num
  , charType     = ctype
  , charHeading  = Nothing
  , charFeature  = feat
  , charUnits    = mUnits
  , charStates   = []
  , charComments = []
  }

-- | Constructor auxiliar para caracteres de texto libre (TE).
mkTextChar :: Int -> Text -> CharacterDef
mkTextChar num feat = CharacterDef
  { charNumber   = num
  , charType     = TE
  , charHeading  = Nothing
  , charFeature  = feat
  , charUnits    = Nothing
  , charStates   = []
  , charComments = []
  }

-- | Determina si el carácter es multiestado discreto (UM, OM, EUM, EOM).
isMultistate :: CharacterDef -> Bool
isMultistate c = case charType c of
  UM  -> True
  OM  -> True
  EUM -> True
  EOM -> True
  _   -> False

-- | Determina si el carácter es cuantitativo continuo o entero (IN, RN).
isNumeric :: CharacterDef -> Bool
isNumeric c = case charType c of
  IN -> True
  RN -> True
  _  -> False

-- | Determina si el carácter prohíbe polimorfismos simultáneos '&' (EUM, EOM).
isExclusive :: CharacterDef -> Bool
isExclusive c = case charType c of
  EUM -> True
  EOM -> True
  _   -> False
