{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Core.Specs
Description : Especificaciones y directivas de control global (*SPECS) de DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Core.Specs
  ( DependentRule (..)
  , ImplicitRule (..)
  , SpecsDef (..)
  , emptySpecs
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import GHC.Generics (Generic)

import Taxide.Core.Types (CharacterType (..), StateId)

-- | Regla de inferencia para caracteres dependientes (*DEPENDENT CHARACTERS).
-- Sintaxis DELTA: @*DEPENDENT CHARACTERS (c,s1/s2:d1-d2:d3)@
-- Si el carácter controlador 'c' toma estados inhabilitantes 's', los dependientes 'd'
-- son forzados a Inapplicable ('-').
data DependentRule = DependentRule
  { depControllerChar  :: !Int         -- ^ Carácter controlador 'c'
  , depDisablingStates :: !(Set StateId) -- ^ Conjunto de estados inhabilitantes: s1, s2...
  , depDependentChars  :: ![Int]       -- ^ Lista de caracteres dependientes afectados: d1..dk
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Regla de valores implícitos (*IMPLICIT VALUES).
-- Sintaxis DELTA: @*IMPLICIT VALUES (c,s:t)@
--   * 's': Estado asignado cuando el carácter cualitativo se omite completamente en el ítem.
--   * 't': Estado asignado cuando el carácter se declara explícitamente pero sin indicar estado (ej. "5,").
data ImplicitRule = ImplicitRule
  { impCharId      :: !Int            -- ^ Carácter 'c'
  , impOmitted     :: !StateId        -- ^ Estado 's' cuando se omite
  , impUnspecified :: !(Maybe StateId) -- ^ Estado 't' cuando se declara vacío
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Definición global de especificaciones taxonómicas (*SPECS).
data SpecsDef = SpecsDef
  { specNumCharacters   :: !Int                      -- ^ *NUMBER OF CHARACTERS
  , specMaxNumberStates :: !Int                      -- ^ *MAXIMUM NUMBER OF STATES
  , specMaxNumberItems  :: !Int                      -- ^ *MAXIMUM NUMBER OF ITEMS
  , specCharTypes       :: !(Map Int CharacterType)  -- ^ *CHARACTER TYPES (c1,UM c2,RN ...)
  , specNumbersOfStates :: !(Map Int Int)            -- ^ *NUMBERS OF STATES (c1,4 c2,2 ...)
  , specDependencies    :: ![DependentRule]          -- ^ *DEPENDENT CHARACTERS
  , specImplicitValues  :: ![ImplicitRule]           -- ^ *IMPLICIT VALUES
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Especificación vacía por defecto.
emptySpecs :: SpecsDef
emptySpecs = SpecsDef
  { specNumCharacters   = 0
  , specMaxNumberStates = 0
  , specMaxNumberItems  = 0
  , specCharTypes       = Map.empty
  , specNumbersOfStates = Map.empty
  , specDependencies    = []
  , specImplicitValues  = []
  }
