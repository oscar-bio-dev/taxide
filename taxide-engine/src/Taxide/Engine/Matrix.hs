{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Engine.Matrix
Description : Transposición a matriz resuelta columnar SoA de alto rendimiento
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Engine.Matrix
  ( PseudoTag
  , tagValid
  , tagUnknown
  , tagInapplicable
  , tagVariable
  , NumericColumn (..)
  , ColumnarMatrix (..)
  , ResolvedMatrix
  , buildColumnarMatrix
  , lookupDiscreteCell
  , lookupNumericCell
  , isCellInapplicable
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (bit, (.|.))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8, Word64)
import GHC.Generics (Generic)

import Taxide.Core.Invariants (collectActiveStates)
import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Core.Types
  ( CharacterType (..)
  , PseudoValue (..)
  , RawAttribute (..)
  , RawStateExpr (..)
  , StatRange (..)
  )
import Taxide.Engine.Variants (AttributedState (..), ResolvedTaxon (..))

-- | Etiqueta de estado para pseudo-valores y validez métrica.
-- 0 = Válido (bitset o medición activa)
-- 1 = Unknown ('U')
-- 2 = Inapplicable ('-')
-- 3 = Variable ('V')
type PseudoTag = Word8

tagValid, tagUnknown, tagInapplicable, tagVariable :: PseudoTag
tagValid        = 0
tagUnknown      = 1
tagInapplicable = 2
tagVariable     = 3

-- | Columna numérica columnar SoA desacoplada en vectores continuos unboxed.
data NumericColumn = NumericColumn
  { ncAbsMin     :: !(VU.Vector Double)
  -- ^ Límite inferior absoluto: (min-)
  , ncNormalLow  :: !(VU.Vector Double)
  -- ^ Límite inferior del rango normal: n1
  , ncCenter     :: !(VU.Vector Double)
  -- ^ Valor central representativo: -med-
  , ncNormalHigh :: !(VU.Vector Double)
  -- ^ Límite superior del rango normal: n2
  , ncAbsMax     :: !(VU.Vector Double)
  -- ^ Límite superior absoluto: (-max)
  , ncPseudoTags :: !(VU.Vector PseudoTag)
  -- ^ Etiquetas de validez (0 = válido, 1 = U, 2 = -, 3 = V)
  } deriving (Eq, Show, Generic, NFData)

-- | Matriz resuelta columnar Structure of Arrays (SoA).
--
-- Almacena cada dimensión morfológica en vectores unboxed contiguos en memoria,
-- eliminando punteros del heap y facilitando operaciones SIMD y bitsets de Fitch en O(1).
data ColumnarMatrix = ColumnarMatrix
  { cmNumTaxa      :: !Int
  -- ^ Número total de taxones N
  , cmNumChars     :: !Int
  -- ^ Número total de caracteres declarados M
  , cmTaxonNames   :: !(Vector Text)
  -- ^ Nombres de los taxones ordenados por índice ordinal
  , cmCharTypes    :: !(Map Int CharacterType)
  -- ^ Mapa de tipos de carácter declarados
  , cmDiscreteCols :: !(Map Int (VU.Vector Word64))
  -- ^ Columnas UM/OM empacadas en bitmasks Word64 (estados 1..62)
  , cmDiscreteTags :: !(Map Int (VU.Vector PseudoTag))
  -- ^ Etiquetas de pseudo-valores por celda en caracteres discretos
  , cmNumericCols  :: !(Map Int NumericColumn)
  -- ^ Columnas IN/RN en vectores unboxed de precisión doble
  , cmTextCols     :: !(Map Int (Vector Text))
  -- ^ Columnas descriptivas TE de texto libre
  } deriving (Eq, Show, Generic, NFData)

-- | Sinónimo de tipo que enfatiza el rol de 'ColumnarMatrix' como matriz resuelta en memoria.
type ResolvedMatrix = ColumnarMatrix

-- | Construye la matriz resuelta columnar SoA a partir de las especificaciones y taxones.
buildColumnarMatrix :: SpecsDef -> [ResolvedTaxon] -> ColumnarMatrix
buildColumnarMatrix specs taxa =
  let nTaxa = length taxa
      nChars = specNumCharacters specs
      names = V.fromList (map rtName taxa)
      typeMap = specCharTypes specs

      allCharIds = [1 .. nChars]

      -- Construir columnas discretas
      discreteEntries = [ (cid, buildDiscreteCol cid taxa)
                        | cid <- allCharIds
                        , isDiscreteType (Map.lookup cid typeMap)
                        ]
      discreteCols = Map.fromList [(cid, fst res) | (cid, res) <- discreteEntries]
      discreteTags = Map.fromList [(cid, snd res) | (cid, res) <- discreteEntries]

      -- Construir columnas numéricas
      numericCols = Map.fromList
        [ (cid, buildNumericCol cid taxa)
        | cid <- allCharIds
        , isNumericType (Map.lookup cid typeMap)
        ]

      -- Construir columnas de texto libre
      textCols = Map.fromList
        [ (cid, buildTextCol cid taxa)
        | cid <- allCharIds
        , Map.lookup cid typeMap == Just TE
        ]

  in ColumnarMatrix
       { cmNumTaxa      = nTaxa
       , cmNumChars     = nChars
       , cmTaxonNames   = names
       , cmCharTypes    = typeMap
       , cmDiscreteCols = discreteCols
       , cmDiscreteTags = discreteTags
       , cmNumericCols  = numericCols
       , cmTextCols     = textCols
       }
  where
    isDiscreteType mType = case mType of
      Just UM  -> True
      Just OM  -> True
      Just EUM -> True
      Just EOM -> True
      Nothing  -> True -- Por defecto si no se especificó
      _        -> False

    isNumericType mType = case mType of
      Just IN -> True
      Just RN -> True
      _       -> False

-- | Empaca una columna cualitativa discreta en un vector unboxed de Word64 y etiquetas.
buildDiscreteCol :: Int -> [ResolvedTaxon] -> (VU.Vector Word64, VU.Vector PseudoTag)
buildDiscreteCol cid taxa =
  let cells = map extractCell taxa
      bitsets = VU.fromList (map fst cells)
      tags    = VU.fromList (map snd cells)
  in (bitsets, tags)
  where
    extractCell taxon =
      case Map.lookup cid (rtAttributes taxon) of
        Nothing ->
          (0, tagUnknown)
        Just (AttributedState raw _) ->
          case raw of
            AttrStates _ expr ->
              case expr of
                PseudoState Unknown _      -> (0, tagUnknown)
                PseudoState Inapplicable _ -> (0, tagInapplicable)
                PseudoState Variable _     -> (0, tagVariable)
                _ ->
                  let states = collectActiveStates expr
                      bitmask = foldl addStateBit 0 states
                  in (bitmask, tagValid)
            _ ->
              (0, tagUnknown)

    addStateBit w s
      | s >= 1 && s <= 62 = w .|. bit (fromIntegral (s - 1))
      | otherwise         = w

-- | Empaca una columna cuantitativa en la estructura NumericColumn SoA.
buildNumericCol :: Int -> [ResolvedTaxon] -> NumericColumn
buildNumericCol cid taxa =
  let rows = map extractNumeric taxa
      absMins = VU.fromList [m | (m, _, _, _, _, _) <- rows]
      nLows   = VU.fromList [l | (_, l, _, _, _, _) <- rows]
      centers = VU.fromList [c | (_, _, c, _, _, _) <- rows]
      nHighs  = VU.fromList [h | (_, _, _, h, _, _) <- rows]
      absMaxs = VU.fromList [x | (_, _, _, _, x, _) <- rows]
      tags    = VU.fromList [t | (_, _, _, _, _, t) <- rows]
  in NumericColumn
       { ncAbsMin     = absMins
       , ncNormalLow  = nLows
       , ncCenter     = centers
       , ncNormalHigh = nHighs
       , ncAbsMax     = absMaxs
       , ncPseudoTags = tags
       }
  where
    extractNumeric taxon =
      case Map.lookup cid (rtAttributes taxon) of
        Nothing ->
          (0.0, 0.0, 0.0, 0.0, 0.0, tagUnknown)
        Just (AttributedState raw _) ->
          case raw of
            AttrReal _ sr _ ->
              let aMin = maybe (srNormalMin sr) id (srAbsoluteMin sr)
                  nLow = srNormalMin sr
                  cen  = maybe ((nLow + srNormalMax sr) / 2.0) id (srCenterVal sr)
                  nHi  = srNormalMax sr
                  aMax = maybe (srNormalMax sr) id (srAbsoluteMax sr)
              in (aMin, nLow, cen, nHi, aMax, tagValid)

            AttrInteger _ sr ->
              let aMin = maybe (fromIntegral (srNormalMin sr)) fromIntegral (srAbsoluteMin sr)
                  nLow = fromIntegral (srNormalMin sr)
                  cen  = maybe ((nLow + fromIntegral (srNormalMax sr)) / 2.0) fromIntegral (srCenterVal sr)
                  nHi  = fromIntegral (srNormalMax sr)
                  aMax = maybe (fromIntegral (srNormalMax sr)) fromIntegral (srAbsoluteMax sr)
              in (aMin, nLow, cen, nHi, aMax, tagValid)

            AttrStates _ (PseudoState p _) ->
              let tag = case p of
                    Unknown      -> tagUnknown
                    Inapplicable -> tagInapplicable
                    Variable     -> tagVariable
              in (0.0, 0.0, 0.0, 0.0, 0.0, tag)

            _ ->
              (0.0, 0.0, 0.0, 0.0, 0.0, tagUnknown)

-- | Empaca una columna descriptiva de texto libre en un vector indexado por taxón.
buildTextCol :: Int -> [ResolvedTaxon] -> Vector Text
buildTextCol cid taxa = V.fromList (map extractText taxa)
  where
    extractText taxon =
      case Map.lookup cid (rtAttributes taxon) of
        Just (AttributedState (AttrText _ txt) _) -> txt
        _                                         -> ""

-- | Consulta el valor de una celda discreta: (Etiqueta, Bitset Word64).
lookupDiscreteCell :: ColumnarMatrix -> Int -> Int -> (PseudoTag, Word64)
lookupDiscreteCell mat taxonIdx cid =
  case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
    (Just cols, Just tags)
      | taxonIdx >= 0 && taxonIdx < cmNumTaxa mat ->
          (tags VU.! taxonIdx, cols VU.! taxonIdx)
    _ ->
      (tagUnknown, 0)

-- | Consulta los parámetros numéricos de una celda: Just (min, normalMin, center, normalMax, max).
lookupNumericCell :: ColumnarMatrix -> Int -> Int -> Maybe (Double, Double, Double, Double, Double)
lookupNumericCell mat taxonIdx cid =
  case Map.lookup cid (cmNumericCols mat) of
    Just nc
      | taxonIdx >= 0 && taxonIdx < cmNumTaxa mat ->
          let tag = ncPseudoTags nc VU.! taxonIdx
          in if tag == tagValid
               then Just ( ncAbsMin nc VU.! taxonIdx
                         , ncNormalLow nc VU.! taxonIdx
                         , ncCenter nc VU.! taxonIdx
                         , ncNormalHigh nc VU.! taxonIdx
                         , ncAbsMax nc VU.! taxonIdx
                         )
               else Nothing
    _ -> Nothing

-- | Determina si una celda ha sido resuelta como Inapplicable ('-').
isCellInapplicable :: ColumnarMatrix -> Int -> Int -> Bool
isCellInapplicable mat taxonIdx cid =
  case Map.lookup cid (cmDiscreteTags mat) of
    Just tags -> (tags VU.! taxonIdx) == tagInapplicable
    Nothing ->
      case Map.lookup cid (cmNumericCols mat) of
        Just nc -> (ncPseudoTags nc VU.! taxonIdx) == tagInapplicable
        Nothing -> False
