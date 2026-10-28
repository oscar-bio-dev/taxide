{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Engine.Linter
Description : Auditoría ontológica, semántica y cladística sobre la matriz resuelta
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Engine.Linter
  ( -- * Diagnósticos Cladísticos y Ontológicos
    lintResolvedMatrix
  , lintInvariantCharacters
  , lintAutapomorphies
  , lintUncodedCharacters
  , lintTaxonDuplicates
  , lintQuantitativeCoherence
  , lintIndistinguishableTaxa
  ) where

import Data.Bits (testBit)
import Data.List (sort, tails)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)

import Taxide.Core.Invariants (validateStatRange)
import Taxide.Core.Item (ItemDef (..), ItemType (..))
import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Core.Types (CharacterType (..), RawAttribute (..))
import Taxide.Engine.Diagnostic
  ( LintDiagnostic
  , autapomorphyDiag
  , duplicateTaxonDiag
  , incoherentRangeDiag
  , indistinguishableTaxaDiag
  , invariantCharDiag
  , nonInformativeCharDiag
  , uncodedCharDiag
  )
import Taxide.Engine.Matrix
  ( ColumnarMatrix (..)
  , NumericColumn (..)
  , ResolvedMatrix
  , tagInapplicable
  , tagUnknown
  , tagValid
  )
import Taxide.Engine.Variants (AttributedState (..), ResolvedTaxon (..))

-- | Ejecuta todas las auditorías cladísticas, de consistencia y ontológicas
-- sobre el dataset y la 'ResolvedMatrix'.
lintResolvedMatrix
  :: SpecsDef
  -> [ItemDef]
  -> [ResolvedTaxon]
  -> ResolvedMatrix
  -> [LintDiagnostic]
lintResolvedMatrix specs items taxa mat =
  concat
    [ lintTaxonDuplicates items
    , lintQuantitativeCoherence taxa
    , lintUncodedCharacters specs mat
    , lintInvariantCharacters mat
    , lintAutapomorphies mat
    , lintIndistinguishableTaxa mat
    ]

-- | TAX-CLAD-001 (Warning): Caracteres Invariantes.
-- Detecta caracteres cualitativos (UM, OM) que presentan idéntico estado
-- en todos los taxones evaluados que no tienen valor Unknown ni Inapplicable.
lintInvariantCharacters :: ResolvedMatrix -> [LintDiagnostic]
lintInvariantCharacters mat =
  let nTaxa = cmNumTaxa mat
      typeMap = cmCharTypes mat
  in [ invariantCharDiag cid (length validVals)
     | (cid, col) <- Map.toList (cmDiscreteCols mat)
     , isQualitative (Map.lookup cid typeMap)
     , let tags = Map.findWithDefault VU.empty cid (cmDiscreteTags mat)
     , let validVals = [ col VU.! i
                       | i <- [0 .. nTaxa - 1]
                       , i < VU.length tags
                       , tags VU.! i == tagValid
                       ]
     , length validVals >= 2
     , case validVals of
         (firstVal : rest) -> all (== firstVal) rest
         []                -> False
     ]

-- | TAX-CLAD-002 (Info): Caracteres No Informativos para Parsimonia.
-- Implementa la Definición 4.1: un carácter discreto es informativo si y solo si
-- al menos dos estados distintos aparecen, cada uno, en al menos dos taxones.
-- Se reporta cualquier carácter variable (con ≥2 estados presentes) que no lo
-- cumple, incluidos los casos con varias autapomorfías en estados distintos.
-- El caso particular de una única autapomorfía frente a un estado basal común
-- conserva su diagnóstico localizado en el taxón autapomórfico.
lintAutapomorphies :: ResolvedMatrix -> [LintDiagnostic]
lintAutapomorphies mat =
  let nTaxa = cmNumTaxa mat
      typeMap = cmCharTypes mat
      names = cmTaxonNames mat
  in concat
     [ case findAutapomorphy validEntries of
         Just (autIdx, countCommon) ->
           let tName = if autIdx < V.length names
                         then names V.! autIdx
                         else "Taxón " <> T.pack (show autIdx)
           in [ autapomorphyDiag cid tName countCommon ]
         Nothing ->
           [ nonInformativeCharDiag cid (Map.size stateCounts) singletons (length validEntries) ]
     | (cid, col) <- Map.toList (cmDiscreteCols mat)
     , isQualitative (Map.lookup cid typeMap)
     , let tags = Map.findWithDefault VU.empty cid (cmDiscreteTags mat)
     , let validEntries = [ (i, col VU.! i)
                          | i <- [0 .. nTaxa - 1]
                          , i < VU.length tags
                          , tags VU.! i == tagValid
                          ]
     , length validEntries >= 2
     , let stateCounts = stateOccurrences (map snd validEntries)
     , let informative = Map.size (Map.filter (>= 2) stateCounts)
     , let singletons = Map.size (Map.filter (== 1) stateCounts)
     , Map.size stateCounts >= 2
     , informative < 2
     ]
  where
    -- Número de taxones en los que aparece cada estado (bit) del carácter.
    stateOccurrences :: [Word64] -> Map.Map Int Int
    stateOccurrences masks =
      Map.fromListWith (+) [ (b, 1) | m <- masks, b <- [0 .. 63], testBit m b ]

    findAutapomorphy :: [(Int, Word64)] -> Maybe (Int, Int)
    findAutapomorphy entries =
      let valGroups = Map.fromListWith (++) [ (val, [idx]) | (idx, val) <- entries ]
      in if length entries >= 3 && Map.size valGroups == 2
           then case [ (val, idxs) | (val, idxs) <- Map.toList valGroups, length idxs == 1 ] of
                  [(_autVal, [autIdx])] ->
                    let countCommon = length entries - 1
                    in Just (autIdx, countCommon)
                  _ -> Nothing
           else Nothing

-- | TAX-DATA-001 (Warning): Caracteres No Codificados.
-- Detecta caracteres definidos en CHARS/SPECS que poseen estado Unknown (U)
-- o Inapplicable (-) en el 100% de los taxones evaluados.
lintUncodedCharacters :: SpecsDef -> ResolvedMatrix -> [LintDiagnostic]
lintUncodedCharacters specs mat =
  let nTaxa = cmNumTaxa mat
      nChars = specNumCharacters specs
  in if nTaxa == 0
       then []
       else
         [ uncodedCharDiag cid nTaxa
         | cid <- [1 .. nChars]
         , isCompletelyUncoded cid
         ]
  where
    isCompletelyUncoded cid =
      case Map.lookup cid (cmDiscreteTags mat) of
        Just tags ->
          all (\t -> t == tagUnknown || t == tagInapplicable) (VU.toList tags)
        Nothing ->
          case Map.lookup cid (cmNumericCols mat) of
            Just numCol ->
              all (\t -> t == tagUnknown || t == tagInapplicable) (VU.toList (ncPseudoTags numCol))
            Nothing ->
              case Map.lookup cid (cmTextCols mat) of
                Just textCol ->
                  all T.null (V.toList textCol)
                Nothing ->
                  True

-- | TAX-DATA-002 (Warning): Taxones Duplicados.
-- Detecta dos o más ítems principales ('#') que comparten exactamente el
-- mismo nombre formal y autoridad taxonómica.
lintTaxonDuplicates :: [ItemDef] -> [LintDiagnostic]
lintTaxonDuplicates items =
  let primaryItems = [item | item <- items, itemType item == PrimaryItem]
      groups = Map.fromListWith (++) [ ((itemName item, itemAuthority item), [item])
                                     | item <- primaryItems
                                     ]
  in [ duplicateTaxonDiag name mAuth (length dupes)
     | ((name, mAuth), dupes) <- Map.toList groups
     , length dupes > 1
     ]

-- | TAX-MATH-001 (Error): Incoherencia en Rangos Cuantitativos.
-- Verifica en caracteres IN y RN la progresión matemática:
-- extremeMin <= normalLow <= centralTend <= normalHigh <= extremeMax.
lintQuantitativeCoherence :: [ResolvedTaxon] -> [LintDiagnostic]
lintQuantitativeCoherence taxa =
  [ incoherentRangeDiag (rtName taxon) cid
  | taxon <- taxa
  , (cid, AttributedState raw _) <- Map.toList (rtAttributes taxon)
  , not (isStatRangeCoherent raw)
  ]
  where
    isStatRangeCoherent raw = case raw of
      AttrReal _ sr _   -> validateStatRange sr
      AttrInteger _ sr  -> validateStatRange sr
      _                 -> True

-- | TAX-WARN-001 (Warning): Taxones Biológicamente Indistinguibles.
-- Detecta pares de taxones con perfiles morfológicos idénticos en todos los
-- caracteres discretos y cuantitativos (ignorando texto libre TE).
lintIndistinguishableTaxa :: ResolvedMatrix -> [LintDiagnostic]
lintIndistinguishableTaxa mat =
  let nTaxa = cmNumTaxa mat
      names = cmTaxonNames mat
      typeMap = cmCharTypes mat
      discreteCids = [ cid
                     | (cid, _) <- Map.toList (cmDiscreteCols mat)
                     , isQualitative (Map.lookup cid typeMap)
                     ]
      numericCids  = Map.keys (cmNumericCols mat)
      -- Firma de fila: etiqueta + valor por columna (las celdas no válidas se
      -- almacenan con valor neutro, por lo que la igualdad de firma equivale a
      -- la comparación celda a celda). Agrupar en un Map evita el coste O(N²).
      groups = Map.fromListWith (flip (++))
                 [ (rowSignature i discreteCids numericCids, [i]) | i <- [0 .. nTaxa - 1] ]
      pairs = sort
                [ (i, j)
                | grp <- Map.elems groups
                , (i : js) <- tails grp
                , j <- js
                ]
  in if (null discreteCids && null numericCids) || nTaxa < 2
       then []
       else [ indistinguishableTaxaDiag (names V.! i) (names V.! j) | (i, j) <- pairs ]
  where
    rowSignature i discreteCids numericCids =
      ( [ (cmDiscreteTags mat Map.! cid VU.! i, cmDiscreteCols mat Map.! cid VU.! i)
        | cid <- discreteCids ]
      , [ ( ncPseudoTags ncol VU.! i
          , ( ncAbsMin ncol VU.! i, ncNormalLow ncol VU.! i, ncCenter ncol VU.! i
            , ncNormalHigh ncol VU.! i, ncAbsMax ncol VU.! i ) )
        | cid <- numericCids
        , let ncol = cmNumericCols mat Map.! cid ]
      )

-- | Determina si un carácter es cualitativo (UM, OM, EUM, EOM o default UM).
isQualitative :: Maybe CharacterType -> Bool
isQualitative (Just UM)  = True
isQualitative (Just OM)  = True
isQualitative (Just EUM) = True
isQualitative (Just EOM) = True
isQualitative Nothing    = True
isQualitative _          = False
