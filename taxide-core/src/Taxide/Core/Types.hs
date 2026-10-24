{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Core.Types
Description : Tipos de datos algebraicos fundamentales y AST crudo de TAXIDE
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Define los tipos fundamentales del dominio taxonómico según el estándar DELTA,
asegurando una separación estricta entre el AST crudo (expresión arbórea sin
aplanar estados ni perder comentarios) y la fase resuelta preparada para cómputo numérico.
-}

module Taxide.Core.Types
  ( -- * Tipos de Caracteres y Pseudo-valores
    CharacterType (..)
  , PseudoValue (..)
  , StateId

    -- * Rangos Cuantitativos y Estadísticos
  , StatRange (..)
  , mkSingleStat
  , mkRangeStat
  , mkFullStatRange

    -- * Expresiones de Estados en el AST Crudo (Preservando sintaxis exacta)
  , StateToken (..)
  , RawStateExpr (..)
  , RawAttribute (..)

    -- * Fase Resuelta (Para Fase 2: Cómputo Vectorizado en Memoria)
  , ResolvedState (..)
  , ResolvedAttribute (..)

    -- * Metadatos para EML (DwC-A)
  , DatasetMetadata (..)
  , emptyDatasetMetadata
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Word (Word8, Word64)
import GHC.Generics (Generic)

-- | Identificador de estado biológico para caracteres discretos (1..64 en estándar DELTA).
type StateId = Word8

-- | Tipos de caracteres morfológicos admitidos por el estándar DELTA (*CHARACTER TYPES).
data CharacterType
  = UM   -- ^ Unordered Multistate (Multiestado desordenado)
  | OM   -- ^ Ordered Multistate (Multiestado ordenado)
  | EUM  -- ^ Exclusive Unordered Multistate (No admite polimorfismo simultáneo '&')
  | EOM  -- ^ Exclusive Ordered Multistate (Ordenado exclusivo)
  | IN   -- ^ Integer Numeric (Numérico entero discreto)
  | RN   -- ^ Real Numeric (Numérico continuo con unidades dimensionales)
  | TE   -- ^ Text (Texto libre descriptivo, no tabulable)
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Pseudo-valores biológicos estándar en matrices morfológicas.
data PseudoValue
  = Unknown      -- ^ 'U': Estado desconocido o no observado
  | Inapplicable -- ^ '-': Carácter no aplicable por dependencia ontogenética/morfológica
  | Variable     -- ^ 'V': Taxón altamente variable que abarca todos los estados
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Rango cuantitativo completo con extremos estadísticos opcionales:
-- @(min-)n1-med-n2(-max)@ o @n@.
data StatRange a = StatRange
  { srAbsoluteMin :: !(Maybe a) -- ^ Extremo inferior absoluto entre paréntesis: (min-)
  , srNormalMin   :: !a         -- ^ Límite inferior del rango normal: n1
  , srCenterVal   :: !(Maybe a) -- ^ Valor central (media, mediana o moda): -med-
  , srNormalMax   :: !a         -- ^ Límite superior del rango normal: n2
  , srAbsoluteMax :: !(Maybe a) -- ^ Extremo superior absoluto entre paréntesis: (-max)
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Constructor para un valor numérico simple @n@ sin rango ni extremos.
mkSingleStat :: a -> StatRange a
mkSingleStat val = StatRange
  { srAbsoluteMin = Nothing
  , srNormalMin   = val
  , srCenterVal   = Nothing
  , srNormalMax   = val
  , srAbsoluteMax = Nothing
  }

-- | Constructor para un rango normal @n1-n2@ sin extremos absolutos.
mkRangeStat :: a -> a -> StatRange a
mkRangeStat n1 n2 = StatRange
  { srAbsoluteMin = Nothing
  , srNormalMin   = n1
  , srCenterVal   = Nothing
  , srNormalMax   = n2
  , srAbsoluteMax = Nothing
  }

-- | Constructor completo para valores cuantitativos complejos.
mkFullStatRange :: Maybe a -> a -> Maybe a -> a -> Maybe a -> StatRange a
mkFullStatRange = StatRange

-- | Token de un estado individual con su comentario contextual opcional: @r1<e1>@.
data StateToken = StateToken
  { stId      :: !StateId
  , stComment :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Árbol de expresiones sintácticas para atributos en el AST crudo de la Fase 1.
-- Modela fielmente la estructura exacta sin aplanar estados, para garantizar la
-- propiedad de idempotencia y round-trip (@parseDelta . renderDelta === Right@).
data RawStateExpr
  = SingleState !StateToken
  -- ^ Un único estado, ej. @1@ o @1<raro>@
  | RangeTo !StateToken !StateToken
  -- ^ Rango sintáctico @1-3@. Nótese que en UM '1-3' indica intermedio entre 1 y 3
  -- excluyendo el 2, mientras que '1-2-3' incluye los tres estados.
  | StateOr !RawStateExpr !RawStateExpr
  -- ^ Operador lógico OR (@/@): Incertidumbre taxonómica, ej. @1/2@
  | StateAnd !RawStateExpr !RawStateExpr
  -- ^ Operador lógico AND (@&@): Polimorfismo simultáneo en el mismo espécimen, ej. @1&2@
  | PseudoState !PseudoValue !(Maybe Text)
  -- ^ Pseudo-valor 'U', '-', 'V', con comentario contextual opcional
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Atributo crudo en el AST de la Fase 1, conteniendo comentarios prefijados y valores.
data RawAttribute
  = AttrStates !(Maybe Text) !RawStateExpr
  -- ^ Atributo cualitativo con comentario de carácter opcional @c<e0>,s1/s2@
  | AttrInteger !(Maybe Text) !(StatRange Int32)
  -- ^ Atributo entero @5,(1-)2-3-4(-5)@
  | AttrReal !(Maybe Text) !(StatRange Double) !(Maybe Text)
  -- ^ Atributo real con unidades opcionales @c<e0>,5.2-6.8<cm>@
  | AttrText !(Maybe Text) !Text
  -- ^ Atributo de texto libre para caracteres tipo TE
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Estado evaluado y unificado en la fase resuelta (para el cómputo en Fase 2).
data ResolvedState
  = ResDiscrete !Word64
  -- ^ Bitmask unboxed de 64 bits para UM/OM (1 bit por cada estado presente 1..64)
  | ResNumeric !Double
  -- ^ Valor real cuantitativo
  | ResInterval !Double !Double
  -- ^ Intervalo numérico [min, max]
  | ResPseudo !PseudoValue
  -- ^ Pseudo-valor evaluado ('U', '-', 'V')
  | ResText !Text
  -- ^ Texto evaluado
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Atributo resuelto en memoria para el motor numérico.
data ResolvedAttribute = ResolvedAttribute
  { resCharId :: !Int
  , resState  :: !ResolvedState
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Metadatos globales del conjunto de datos para la generación del archivo EML
-- (Ecological Metadata Language) en Darwin Core Archive (DwC-A).
data DatasetMetadata = DatasetMetadata
  { datasetTitle       :: !Text          -- ^ Título descriptivo del proyecto o monografía
  , datasetCreators    :: ![Text]        -- ^ Lista de autores, taxónomos o curadores
  , datasetAbstract    :: !(Maybe Text)  -- ^ Resumen biológico o alcance taxonómico
  , datasetLicense     :: !(Maybe Text)  -- ^ Licencia de datos abiertos (ej. "CC-BY-4.0", "CC0")
  , datasetContact     :: !(Maybe Text)  -- ^ Información de contacto o correo institucional
  , datasetCitation    :: !(Maybe Text)  -- ^ Cita bibliográfica formal sugerida
  , datasetIdentifier  :: !(Maybe Text)  -- ^ Identificador persistente o DOI
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Constructor por defecto para metadatos con un título dado.
emptyDatasetMetadata :: Text -> DatasetMetadata
emptyDatasetMetadata title = DatasetMetadata
  { datasetTitle       = title
  , datasetCreators    = []
  , datasetAbstract    = Nothing
  , datasetLicense     = Just "CC-BY-4.0"
  , datasetContact     = Nothing
  , datasetCitation    = Nothing
  , datasetIdentifier  = Nothing
  }
