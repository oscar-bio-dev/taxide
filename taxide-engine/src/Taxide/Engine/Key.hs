{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Engine.Key
Description : Generador de claves dicotómicas óptimas mediante Entropía de Shannon
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Implementa la Sección 5.6 de la especificación formal (reemplazo del ejecutable @KEY@
de Dallwitz, 1974):
1. Selección recursiva en cada nodo de decisión de la bipartición dicotómica (binaria)
   que maximiza la ganancia de información de Shannon penalizada por duplicación
   polimórfica y costo de observación $\mathrm{Score}(c; \mathcal{T}')$.
2. Discretización local por umbral óptimo sobre el subconjunto activo $\mathcal{T}'$ para
   caracteres cuantitativos enteros (@IN@, con cortes enteros sin decimales artificiales)
   y continuos (@RN@, con intervalos reales exactos), más enriquecimiento con caracteres
   co-diagnósticos equivalentes.
3. Propagación completa de taxones polimórficos o con incertidumbre hacia todas sus
   ramas compatibles, garantizando que el 100% de los taxones sean alcanzables.
4. Renderizadores puros hacia Clave Pareada / Bracketed, Clave Indentada y Markdown,
   con numeración secuencial exacta de dilemas (@1(0).@ / @1'.@) y formato limpio de
   comentarios DELTA, soportando inglés ('LangEn') y español ('LangEs').
-}

module Taxide.Engine.Key
  ( -- * Configuración e Idioma
    KeyLang (..)
  , KeyOptions (..)
  , defaultKeyOptions

    -- * Árbol de Decisión de Identificación
  , KeyBranch (..)
  , KeyDecision (..)
  , KeyTree (..)
  , keyLeafTaxa

    -- * Construcción y Evaluación por Entropía de Shannon
  , buildIdentificationKey
  , shannonEntropy
  , cleanDeltaComments

    -- * Renderizadores Puros
  , renderBracketedKey
  , renderIndentedKey
  , renderMarkdownKey
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, testBit, (.|.))
import Data.Char (toLower, toUpper)
import Data.List (maximumBy, nub, sort, sortBy)
#if __GLASGOW_HASKELL__ < 910
import Data.List (foldl')
#endif
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric (showFFloat)

import Taxide.Core.Character (CharacterDef (..))
import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Core.Types (CharacterType (..), StateId)
import Taxide.Engine.Discretize
  ( DiscretizeMethod (..)
  , DiscretizeTarget (..)
  )
import Taxide.Engine.Matrix
  ( ColumnarMatrix (..)
  , NumericColumn (..)
  , PseudoTag
  , maxBitsetStates
  , tagInapplicable
  , tagValid
  )
import Taxide.Engine.Variants (ResolvedTaxon (..))

-- | Idioma de salida para los encabezados y conectores de la clave de identificación.
data KeyLang
  = LangEn
  -- ^ Inglés ("Identification Key", etc.)
  | LangEs
  -- ^ Español ("Clave de Identificación", etc.)
  deriving (Eq, Ord, Show, Read, Generic, NFData)

-- | Opciones de construcción de la clave dicotómica.
data KeyOptions = KeyOptions
  { koDiscretizeMethod :: !DiscretizeMethod
  -- ^ Regla estadística para discretizar caracteres cuantitativos (@IN@, @RN@)
  , koDiscretizeTarget :: !DiscretizeTarget
  -- ^ Valor representativo del 'StatRange'
  , koCharWeights      :: !(Map Int Double)
  -- ^ Costos empíricos de observación $w_c > 0$ por carácter (por defecto inferidos según tipo/fiabilidad)
  } deriving (Eq, Show, Generic, NFData)

-- | Opciones por defecto: regla de la raíz cuadrada, valor central y pesos diagnósticos automáticos.
defaultKeyOptions :: KeyOptions
defaultKeyOptions = KeyOptions
  { koDiscretizeMethod = SquareRootRule
  , koDiscretizeTarget = UseCenterVal
  , koCharWeights      = Map.empty
  }

-- | Rama individual saliente de un nodo de decisión en la clave.
data KeyBranch = KeyBranch
  { kbStateId    :: !StateId
  , kbStateLabel :: !Text
  , kbSubtree    :: !KeyTree
  } deriving (Eq, Show, Generic, NFData)

-- | Nodo de decisión caracterizado por el carácter discriminante y sus métricas de Shannon.
data KeyDecision = KeyDecision
  { kdCharId   :: !Int
  , kdFeature  :: !Text
  , kdEntropy  :: !Double
  , kdGain     :: !Double
  , kdScore    :: !Double
  , kdBranches :: ![KeyBranch]
  } deriving (Eq, Show, Generic, NFData)

-- | Árbol de decisión que modela una clave de identificación taxonómica.
data KeyTree
  = KeyLeaf ![Text]
  -- ^ Hoja terminal con el taxón identificado (o lista de taxones si son indistinguibles)
  | KeyNode !KeyDecision
  -- ^ Bifurcación basada en los estados de un carácter
  deriving (Eq, Show, Generic, NFData)

-- | Devuelve la lista deduplicada de todos los nombres de taxones presentes en las hojas
-- de la clave generada.
keyLeafTaxa :: KeyTree -> [Text]
keyLeafTaxa tree = nub (collect tree)
  where
    collect (KeyLeaf names) = names
    collect (KeyNode dec)   = concatMap (collect . kbSubtree) (kdBranches dec)

-- | Entropía basal de Shannon $H(\mathcal{T}') = \log_2 |\mathcal{T}'|$ para un conjunto de
-- cardinalidad $n$.
shannonEntropy :: Int -> Double
shannonEntropy n
  | n <= 1    = 0.0
  | otherwise = logBase 2.0 (fromIntegral n)

-- | Limpia comentarios en notación DELTA @<comentario>@ convirtiéndolos a paréntesis
-- legibles @(comentario)@.
cleanDeltaComments :: Text -> Text
cleanDeltaComments raw =
  let step (acc, prevSpace) ch
        | ch == '<' =
            let pfx = if T.null acc || prevSpace then "(" else " ("
            in (acc <> pfx, False)
        | ch == '>' =
            (acc <> ")", False)
        | otherwise =
            (acc <> T.singleton ch, ch == ' ')
  in T.strip (fst (T.foldl' step ("", True) (T.strip raw)))

-- | Capitaliza la primera letra de un texto manteniendo el resto intacto.
capitalizeFirst :: Text -> Text
capitalizeFirst txt =
  case T.uncons (T.strip txt) of
    Nothing      -> ""
    Just (c, cs) -> T.cons (toUpper c) cs

-- | Pone en minúscula la primera letra de un texto (salvo siglas).
lowercaseFirst :: Text -> Text
lowercaseFirst txt =
  case T.uncons (T.strip txt) of
    Nothing      -> ""
    Just (c, cs) -> T.cons (toLower c) cs

-- | Une una lista de etiquetas de estados con @", "@ y @" o "@.
joinStateLabels :: [Text] -> Text
joinStateLabels []        = ""
joinStateLabels [x]       = x
joinStateLabels [x, y]    = x <> " o " <> y
joinStateLabels (x : xs)  =
  case reverse xs of
    []           -> x
    (lastX : revMid) ->
      T.intercalate ", " (x : reverse revMid) <> " o " <> lastX

-- | Carácter discreto candidato preparado para la clave.
data DiscreteCandidate = DiscreteCandidate
  { dcId         :: !Int
  , dcType       :: !CharacterType
  , dcFeature    :: !Text
  , dcFeatureRaw :: !Text
  , dcWeight     :: !Double
  , dcStates     :: ![(StateId, Text)]
  , dcTaxonMasks :: !(VU.Vector Word64)
  }

-- | Carácter cuantitativo candidato preparado para la clave.
data QuantCandidate = QuantCandidate
  { qcId         :: !Int
  , qcType       :: !CharacterType
  , qcFeature    :: !Text
  , qcFeatureRaw :: !Text
  , qcUnits      :: !Text
  , qcWeight     :: !Double
  , qcNormalLow  :: !(VU.Vector Double)
  , qcCenter     :: !(VU.Vector Double)
  , qcNormalHigh :: !(VU.Vector Double)
  , qcTags       :: !(VU.Vector PseudoTag)
  }

-- | Representación interna de un carácter candidato (discreto o cuantitativo) para la clave.
data CandidateChar
  = CandDiscrete !DiscreteCandidate
  | CandQuant    !QuantCandidate

candCharId :: CandidateChar -> Int
candCharId (CandDiscrete dc) = dcId dc
candCharId (CandQuant qc)    = qcId qc

candFeature :: CandidateChar -> Text
candFeature (CandDiscrete dc) = dcFeature dc
candFeature (CandQuant qc)    = qcFeature qc

candFeatureRaw :: CandidateChar -> Text
candFeatureRaw (CandDiscrete dc) = dcFeatureRaw dc
candFeatureRaw (CandQuant qc)    = qcFeatureRaw qc

candIsQualitative :: CandidateChar -> Bool
candIsQualitative (CandDiscrete _) = True
candIsQualitative (CandQuant _)    = False

-- | Bipartición candidata evaluada sobre un subconjunto de taxones $\mathcal{T}'$.
data EvaluatedSplit = EvaluatedSplit
  { esScore    :: !Double
  , esGain     :: !Double
  , esCand     :: !CandidateChar
  , esBranches :: ![(StateId, Text, [Int])]
  }

-- | Construye recursivamente la clave dicotómica de identificación seleccionando en cada
-- paso la bipartición binaria que maximiza $\mathrm{Score}(c; \mathcal{T}')$ (Ecuación 5.17).
buildIdentificationKey
  :: KeyOptions
  -> SpecsDef
  -> [CharacterDef]
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> KeyTree
buildIdentificationKey opts specs chars taxa mat =
  let nTaxa = cmNumTaxa mat
      candidates = prepareCandidateChars opts specs chars taxa mat
      allTaxaIndices = [0 .. nTaxa - 1]
  in if nTaxa <= 0
       then KeyLeaf []
       else buildNode candidates Set.empty allTaxaIndices
  where
    taxonName :: Int -> Text
    taxonName idx
      | idx >= 0 && idx < V.length (cmTaxonNames mat) = cmTaxonNames mat V.! idx
      | otherwise = "Taxon_" <> T.pack (show idx)

    buildNode :: [CandidateChar] -> Set (Int, [Int], [Int]) -> [Int] -> KeyTree
    buildNode _ _ [] = KeyLeaf []
    buildNode _ _ [singleIdx] = KeyLeaf [taxonName singleIdx]
    buildNode candidates usedSplits subset =
      let allSplits =
            [ ev
            | c <- candidates
            , ev <- evaluateCandidateSplits c subset
            , case esBranches ev of
                [(_, _, t1), (_, _, t2)] ->
                  Set.notMember (candCharId c, t1, t2) usedSplits
                _ -> True
            ]
      in case allSplits of
           [] ->
             KeyLeaf (map taxonName subset)
           _ ->
             let bestSplit =
                   maximumBy
                     (comparing (\ev ->
                       ( esScore ev
                       , esGain ev
                       , candIsQualitative (esCand ev)
                       , negate (candCharId (esCand ev))
                       )))
                     allSplits
                 bestChar     = esCand bestSplit
                 bestBranches = orientBinaryBranches (esBranches bestSplit)
                 enrichedBranches = enrichWithCoDiagnostic bestChar bestBranches allSplits
                 usedSplits' = case bestBranches of
                   [(_, _, t1), (_, _, t2)] ->
                     Set.insert (candCharId bestChar, t1, t2)
                       (Set.insert (candCharId bestChar, t2, t1) usedSplits)
                   _ -> usedSplits
                 hBase = shannonEntropy (length subset)
                 childBranches =
                   [ KeyBranch
                       { kbStateId    = stId
                       , kbStateLabel = stLbl
                       , kbSubtree    = buildNode candidates usedSplits' subTaxa
                       }
                   | (stId, stLbl, subTaxa) <- enrichedBranches
                   ]
             in KeyNode KeyDecision
                  { kdCharId   = candCharId bestChar
                  , kdFeature  = candFeature bestChar
                  , kdEntropy  = hBase
                  , kdGain     = esGain bestSplit
                  , kdScore    = esScore bestSplit
                  , kdBranches = childBranches
                  }

-- | Orienta las dos ramas de un dilema binario para que el subconjunto menor (la especie
-- o clado que se separa primero) aparezca en la primera proposición @n(p).@ y el grupo
-- restante continúe en @n'.@, manteniendo el orden natural cuando ambos grupos tienen
-- igual cardinalidad.
orientBinaryBranches :: [(StateId, Text, [Int])] -> [(StateId, Text, [Int])]
orientBinaryBranches [b1@(_, _, t1), b2@(_, _, t2)]
  | length t1 > length t2 = [b2, b1]
  | otherwise             = [b1, b2]
orientBinaryBranches bs = bs

-- | Enriquece las proposiciones de un dilema binario con hasta 1 carácter co-diagnóstico
-- cualitativo o merístico adicional que induzca exactamente la misma partición de taxones
-- $(T_1, T_2)$ en el nodo actual (estilo Reynolds 2017 / Glasby et al. 2025), siempre que
-- haya más de 2 taxones en el subconjunto.
enrichWithCoDiagnostic
  :: CandidateChar
  -> [(StateId, Text, [Int])]
  -> [EvaluatedSplit]
  -> [(StateId, Text, [Int])]
enrichWithCoDiagnostic primaryChar [b1@(s1, lbl1, t1), b2@(s2, lbl2, t2)] allSplits
  | length t1 + length t2 <= 2 = [b1, b2]
  | otherwise =
      let matchingCoChars =
            nubByCharId $
              sortBy (comparing (\(_, _, c) -> (not (candIsQualitative c), candCharId c)))
                [ (coLbl1, coLbl2, coCand)
                | ev <- allSplits
                , let coCand = esCand ev
                , candCharId coCand /= candCharId primaryChar
                , candIsQualitative coCand
                , (coLbl1, coLbl2) <- matchPartition (t1, t2) (esBranches ev)
                ]
      in case take 1 matchingCoChars of
           [] -> [b1, b2]
           ((co1, co2, coCand) : _) ->
             let featLower = lowercaseFirst (candFeatureRaw coCand)
                 lbl1' = lbl1 <> "; " <> featLower <> ": " <> co1
                 lbl2' = lbl2 <> "; " <> featLower <> ": " <> co2
             in [(s1, lbl1', t1), (s2, lbl2', t2)]
  where
    matchPartition (target1, target2) [(_, lA, txA), (_, lB, txB)]
      | txA == target1 && txB == target2 = [(lA, lB)]
      | txB == target1 && txA == target2 = [(lB, lA)]
    matchPartition _ _ = []

    nubByCharId [] = []
    nubByCharId (x@(_, _, c) : xs) =
      x : nubByCharId [y | y@(_, _, c') <- xs, candCharId c' /= candCharId c]
enrichWithCoDiagnostic _ bs _ = bs

-- | Evalúa todas las biparticiones binarias válidas de un carácter candidato sobre el
-- subconjunto de taxones $\mathcal{T}'$, verificando completitud ($\mathcal{T}'_1 \cup \mathcal{T}'_2 = \mathcal{T}'$)
-- y reducción estricta ($0 < |\mathcal{T}'_s| < |\mathcal{T}'|$).
evaluateCandidateSplits :: CandidateChar -> [Int] -> [EvaluatedSplit]
evaluateCandidateSplits cand subset =
  case cand of
    CandDiscrete dc -> evaluateDiscreteSplits cand dc subset
    CandQuant qc    -> evaluateQuantSplits cand qc subset

-- | Evalúa biparticiones binarias de un carácter discreto (@UM@, @EUM@, @OM@, @EOM@).
evaluateDiscreteSplits :: CandidateChar -> DiscreteCandidate -> [Int] -> [EvaluatedSplit]
evaluateDiscreteSplits cand dc subset =
  let nSub = length subset
      subsetSet = Set.fromList subset
      activeStates =
        [ (stId, stLbl, compatibleTaxa)
        | (stId, stLbl) <- dcStates dc
        , let bitIdx = fromIntegral stId - 1
        , let compatibleTaxa =
                [ tIdx
                | tIdx <- subset
                , tIdx >= 0 && tIdx < VU.length (dcTaxonMasks dc)
                , testBit (dcTaxonMasks dc VU.! tIdx) bitIdx
                ]
        , not (null compatibleTaxa)
        ]
      coveredSet = Set.fromList (concat [ts | (_, _, ts) <- activeStates])
      k = length activeStates
      bipartitions
        | k < 2 || coveredSet /= subsetSet = []
        | k == 2 =
            case activeStates of
              [a, b] -> [([a], [b])]
              _      -> []
        | dcType dc `elem` [OM, EOM] =
            [ splitAt p activeStates | p <- [1 .. k - 1] ]
        | otherwise =
            let singletons =
                  [ ( [st]
                    , take i activeStates ++ drop (i + 1) activeStates
                    )
                  | (i, st) <- zip [0 ..] activeStates
                  ]
                pairs4 = case activeStates of
                  [s0, s1, s2, s3] ->
                    [ ([s0, s1], [s2, s3])
                    , ([s0, s2], [s1, s3])
                    , ([s0, s3], [s1, s2])
                    ]
                  _ -> []
            in singletons ++ pairs4
  in [ ev
     | (grpA, grpB) <- bipartitions
     , Just ev <- [scoreBinaryGroup cand (dcWeight dc) nSub subset grpA grpB]
     ]

scoreBinaryGroup
  :: CandidateChar
  -> Double
  -> Int
  -> [Int]
  -> [(StateId, Text, [Int])]
  -> [(StateId, Text, [Int])]
  -> Maybe EvaluatedSplit
scoreBinaryGroup cand candW nSub subset grpA grpB =
  let setA  = Set.fromList (concat [ts | (_, _, ts) <- grpA])
      setB  = Set.fromList (concat [ts | (_, _, ts) <- grpB])
      taxaA = [t | t <- subset, Set.member t setA]
      taxaB = [t | t <- subset, Set.member t setB]
      stIdA = case grpA of ((s, _, _) : _) -> s; [] -> 1
      stIdB = case grpB of ((s, _, _) : _) -> s; [] -> 2
      lblA  = joinStateLabels [l | (_, l, _) <- grpA]
      lblB  = joinStateLabels [l | (_, l, _) <- grpB]
      stateComplexityPenalty =
        if length grpA == 1 && length grpB == 1 then 1.0 else 1.04
  in buildEvaluatedSplit cand (candW * stateComplexityPenalty) nSub
       [(stIdA, lblA, taxaA), (stIdB, lblB, taxaB)]

-- | Evalúa biparticiones por umbral óptimo local sobre caracteres cuantitativos (@IN@, @RN@).
evaluateQuantSplits :: CandidateChar -> QuantCandidate -> [Int] -> [EvaluatedSplit]
evaluateQuantSplits cand qc subset =
  let nSub  = length subset
      ptags = qcTags qc
      nLows = qcNormalLow qc
      cens  = qcCenter qc
      nHis  = qcNormalHigh qc
      hasInapplicable =
        any (\tIdx -> tIdx >= 0 && tIdx < VU.length ptags && ptags VU.! tIdx == tagInapplicable) subset
      validRows =
        [ (tIdx, nLows VU.! tIdx, cens VU.! tIdx, nHis VU.! tIdx)
        | tIdx <- subset
        , tIdx >= 0 && tIdx < VU.length ptags
        , ptags VU.! tIdx == tagValid
        ]
      distinctCenters = nub (sort [c | (_, _, c, _) <- validRows])
  in if hasInapplicable || length validRows < 2 || length distinctCenters < 2
       then []
       else
         let vMin = minimum [lo | (_, lo, _, _) <- validRows]
             vMax = maximum [hi | (_, _, _, hi) <- validRows]
             adjPairs = zip distinctCenters (drop 1 distinctCenters)
         in [ ev
            | (c1, c2) <- adjPairs
            , Just ev <- [evalThreshold cand qc nSub subset validRows vMin vMax c1 c2]
            ]

evalThreshold
  :: CandidateChar
  -> QuantCandidate
  -> Int
  -> [Int]
  -> [(Int, Double, Double, Double)]
  -> Double
  -> Double
  -> Double
  -> Double
  -> Maybe EvaluatedSplit
evalThreshold cand qc nSub subset validRows vMin vMax c1 c2 =
  let ptags = qcTags qc
      nLows = qcNormalLow qc
      cens  = qcCenter qc
      nHis  = qcNormalHigh qc
      units = qcUnits qc
  in case qcType qc of
       IN ->
         let cutInt = floor ((c1 + c2) / 2.0) :: Integer
             cutD   = fromIntegral cutInt :: Double
             taxaA  =
               [ tIdx
               | tIdx <- subset
               , let isVal = ptags VU.! tIdx == tagValid
               , not isVal || nLows VU.! tIdx <= cutD || cens VU.! tIdx <= cutD
               ]
             taxaB  =
               [ tIdx
               | tIdx <- subset
               , let isVal = ptags VU.! tIdx == tagValid
               , not isVal || nHis VU.! tIdx >= cutD + 1.0 || cens VU.! tIdx > cutD
               ]
             setA = Set.fromList taxaA
             setB = Set.fromList taxaB
             valsA = [(lo, hi) | (tIdx, lo, _, hi) <- validRows, Set.member tIdx setA]
             valsB = [(lo, hi) | (tIdx, lo, _, hi) <- validRows, Set.member tIdx setB]
         in if null valsA || null valsB
              then Nothing
              else
                let minA = round (minimum (map fst valsA)) :: Integer
                    maxA = round (maximum (map snd valsA)) :: Integer
                    minB = round (minimum (map fst valsB)) :: Integer
                    maxB = round (maximum (map snd valsB)) :: Integer
                    lblA = fmtIntRange minA maxA <> " (≤ " <> T.pack (show cutInt) <> ")" <> units
                    lblB = fmtIntRange minB maxB <> " (≥ " <> T.pack (show (cutInt + 1)) <> ")" <> units
                in buildEvaluatedSplit cand (qcWeight qc) nSub
                     [(1, lblA, taxaA), (2, lblB, taxaB)]
       _ ->
         let cut   = (c1 + c2) / 2.0
             taxaA =
               [ tIdx
               | tIdx <- subset
               , let isVal = ptags VU.! tIdx == tagValid
               , not isVal || cens VU.! tIdx < cut
               ]
             taxaB =
               [ tIdx
               | tIdx <- subset
               , let isVal = ptags VU.! tIdx == tagValid
               , not isVal || cens VU.! tIdx >= cut
               ]
             lblA = "[" <> fmtDouble vMin <> ", " <> fmtDouble cut <> ")" <> units
             lblB = "[" <> fmtDouble cut <> ", " <> fmtDouble vMax <> "]" <> units
         in buildEvaluatedSplit cand (qcWeight qc) nSub
              [(1, lblA, taxaA), (2, lblB, taxaB)]

fmtIntRange :: Integer -> Integer -> Text
fmtIntRange lo hi
  | lo == hi  = T.pack (show lo)
  | otherwise = T.pack (show lo) <> "–" <> T.pack (show hi)

buildEvaluatedSplit
  :: CandidateChar
  -> Double
  -> Int
  -> [(StateId, Text, [Int])]
  -> Maybe EvaluatedSplit
buildEvaluatedSplit cand effWeight nSub branches =
  let coveredSet = Set.fromList (concat [ts | (_, _, ts) <- branches])
      allNonEmptyAndStrict = all (\(_, _, ts) -> not (null ts) && length ts < nSub) branches
  in if length branches < 2 || Set.size coveredSet < nSub || not allNonEmptyAndStrict
       then Nothing
       else
         let !hBase = shannonEntropy nSub
             !totalBranchSizes = sum [length ts | (_, _, ts) <- branches]
             !totalD = fromIntegral totalBranchSizes :: Double
             !expectedChildEntropy = sum
               [ let !sz  = length ts
                     !szD = fromIntegral sz :: Double
                 in (szD / totalD) * shannonEntropy sz
               | (_, _, ts) <- branches
               ]
             !gain = max 1.0e-9 (hBase - expectedChildEntropy)
             !dupPenalty = (totalD / fromIntegral nSub) ** 2.0
             !w = max 1.0e-6 effWeight
             !score = gain / (w * dupPenalty)
         in Just EvaluatedSplit
              { esScore    = score
              , esGain     = gain
              , esCand     = cand
              , esBranches = branches
              }

-- | Prepara todos los caracteres discretos y cuantitativos para la construcción de la clave.
prepareCandidateChars
  :: KeyOptions
  -> SpecsDef
  -> [CharacterDef]
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> [CandidateChar]
prepareCandidateChars opts specs chars _taxa mat =
  let charMap = Map.fromList [(charNumber c, c) | c <- chars]
  in concatMap (buildCand charMap) [1 .. cmNumChars mat]
  where
    nTaxa   = cmNumTaxa mat
    typeMap = cmCharTypes mat

    rawFeatureOf charMap cid =
      case Map.lookup cid charMap of
        Just c | not (T.null (T.strip (charFeature c))) ->
          cleanDeltaComments (charFeature c)
        _ -> "character " <> T.pack (show cid)

    featureOf charMap cid =
      capitalizeFirst (rawFeatureOf charMap cid)

    stateLabelOf charMap cid stId =
      case Map.lookup cid charMap >>= lookup stId . charStates of
        Just lbl | not (T.null (T.strip lbl)) -> cleanDeltaComments lbl
        _ -> "state " <> T.pack (show stId)

    unitsOf charMap cid =
      case Map.lookup cid charMap >>= charUnits of
        Just u | not (T.null (T.strip u)) -> " " <> T.strip u
        _ -> ""

    -- Costo intrínseco de observación por defecto cuando el usuario no lo fija en koCharWeights:
    -- 1. Caracteres de jerarquía taxonómica (familia, orden, clase, filo, género): w = 0.15
    -- 2. Caracteres morfológicos cualitativos discretos (UM, OM, EUM, EOM): w = 1.00
    -- 3. Conteos merísticos enteros (IN, ej. segmentos del clitelo / TP): w = 1.15
    -- 4. Caracteres ecológicos / de hábitat: w = 1.30
    -- 5. Mediciones morfométricas continuas (RN, ej. longitud corporal): w = 2.00
    weightOf charMap cid cType =
      case Map.lookup cid (koCharWeights opts) of
        Just w | w > 0.0 -> w
        _ ->
          let featLower = T.toLower (rawFeatureOf charMap cid)
              isTaxRank = any (`T.isInfixOf` featLower)
                ["familia", "family", "orden", "order", "clase", "class", "filo", "phylum", "género", "genus"]
              isHabitat = any (`T.isInfixOf` featLower)
                ["hábitat", "habitat", "ecológic", "ecologic"]
          in if isTaxRank
               then 0.15
               else case cType of
                      IN -> 1.15
                      RN -> 2.00
                      _  -> if isHabitat then 1.30 else 1.00

    buildCand charMap cid =
      let cType = Map.findWithDefault UM cid typeMap
      in case cType of
           TE -> []
           IN -> buildQuantitative charMap cid IN
           RN -> buildQuantitative charMap cid RN
           _  -> buildDiscrete charMap cid cType

    buildDiscrete charMap cid cType =
      case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
        (Just col, Just tags) ->
          let mDecl = Map.findWithDefault 0 cid (specNumbersOfStates specs)
              fromChars = case Map.lookup cid charMap of
                Just c  -> maximum (0 : map (fromIntegral . fst) (charStates c))
                Nothing -> 0
              unionMask = foldl'
                (\acc i -> if tags VU.! i == tagValid then acc .|. (col VU.! i) else acc)
                0
                [0 .. min (VU.length col) (VU.length tags) - 1]
              maxObs = maximum (0 : [b + 1 | b <- [0 .. maxBitsetStates - 1], testBit unionMask b])
              !m = max 2 (maximum [mDecl, fromChars, maxObs])
              !fullMask = if m >= maxBitsetStates then complementZero else (1 `shiftL` m) - 1
              statesList =
                [ (fromIntegral s, stateLabelOf charMap cid (fromIntegral s))
                | s <- [1 .. m]
                ]
              masks = VU.generate nTaxa $ \i ->
                if i < VU.length tags
                  then
                    let tag = tags VU.! i
                        msk = col VU.! i
                    in if tag == tagValid && msk /= 0
                         then msk
                         else if tag == tagInapplicable
                           then 0
                           else fullMask
                  else fullMask
          in [ CandDiscrete $ DiscreteCandidate
                 { dcId         = cid
                 , dcType       = cType
                 , dcFeature    = featureOf charMap cid
                 , dcFeatureRaw = rawFeatureOf charMap cid
                 , dcWeight     = weightOf charMap cid cType
                 , dcStates     = statesList
                 , dcTaxonMasks = masks
                 }
             ]
        _ -> []

    complementZero :: Word64
    complementZero = maxBound

    buildQuantitative charMap cid cType =
      case Map.lookup cid (cmNumericCols mat) of
        Nothing -> []
        Just nc ->
          [ CandQuant $ QuantCandidate
              { qcId         = cid
              , qcType       = cType
              , qcFeature    = featureOf charMap cid
              , qcFeatureRaw = rawFeatureOf charMap cid
              , qcUnits      = unitsOf charMap cid
              , qcWeight     = weightOf charMap cid cType
              , qcNormalLow  = ncNormalLow nc
              , qcCenter     = ncCenter nc
              , qcNormalHigh = ncNormalHigh nc
              , qcTags       = ncPseudoTags nc
              }
          ]

fmtDouble :: Double -> Text
fmtDouble d
  | d == fromIntegral (round d :: Integer) = T.pack (show (round d :: Integer))
  | otherwise =
      let raw = T.pack (showFFloat (Just 2) d "")
          trimmed = T.dropWhileEnd (== '0') raw
      in if T.isSuffixOf "." trimmed then T.dropEnd 1 trimmed else trimmed

-- | Formatea el enunciado completo de una proposición como @"Característica: estado"@.
formatLeadStatement :: Text -> Text -> Text
formatLeadStatement feat stLbl
  | T.null feat  = stLbl
  | T.null stLbl = feat
  | otherwise    = feat <> ": " <> stLbl

--------------------------------------------------------------------------------
-- Numeración Secuencial de Dilemas y Renderizadores Puros
--------------------------------------------------------------------------------

-- | Asigna números de dilema consecutivos ($1, 2, \dots, K$) en recorrido pre-orden,
-- garantizando que cada referencia @Right childNum@ coincida exactamente con el encabezado
-- @childNum(parentNum).@ del dilema destino.
numberCouplets :: Int -> Int -> KeyTree -> (Int, [(Int, Int, Text, [(Text, Either [Text] Int)])])
numberCouplets nextNum _ (KeyLeaf _) = (nextNum, [])
numberCouplets myNum parentNum (KeyNode dec) =
  let (nextAfterChildren, revLeads, childCoupletsList) =
        foldl'
          (\(currNext, accLeads, accCouplets) br ->
            let leadText = formatLeadStatement (kdFeature dec) (kbStateLabel br)
            in case kbSubtree br of
                 KeyLeaf names ->
                   (currNext, (leadText, Left names) : accLeads, accCouplets)
                 sub@(KeyNode _) ->
                   let childNum = currNext
                       (next', subCouplets) = numberCouplets childNum myNum sub
                   in (next', (leadText, Right childNum) : accLeads, accCouplets ++ subCouplets)
          )
          (myNum + 1, [], [])
          (kdBranches dec)
      thisCouplet = (myNum, parentNum, kdFeature dec, reverse revLeads)
  in (nextAfterChildren, thisCouplet : childCoupletsList)

-- | Renderiza la clave en formato clásico Pareado / Bracketed (numeración @1(0).@ / @1'.@).
renderBracketedKey :: KeyLang -> KeyTree -> Text
renderBracketedKey _ (KeyLeaf []) = ""
renderBracketedKey _ (KeyLeaf taxa) = T.intercalate " / " taxa <> "\n"
renderBracketedKey _ rootNode@(KeyNode _) =
  let (_, couplets) = numberCouplets 1 0 rootNode
      renderedGroups = map renderCouplet couplets
  in T.unlines (intercalateBlank renderedGroups)
  where
    intercalateBlank []       = []
    intercalateBlank [g]      = g
    intercalateBlank (g : gs) = g ++ [""] ++ intercalateBlank gs

    renderCouplet :: (Int, Int, Text, [(Text, Either [Text] Int)]) -> [Text]
    renderCouplet (num, parentNum, _, leads) =
      let prefixFirst = T.pack (show num) <> "(" <> T.pack (show parentNum) <> "). "
          mkPrefix idx
            | idx == (0 :: Int) = prefixFirst
            | otherwise =
                let primes = T.replicate idx "'"
                    rawPfx = T.pack (show num) <> primes <> ". "
                    padLen = max 0 (T.length prefixFirst - T.length rawPfx)
                in rawPfx <> T.replicate padLen " "
      in [ mkPrefix idx <> leadTxt <> " ... " <> renderTarget target
         | (idx, (leadTxt, target)) <- zip [0 ..] leads
         ]

    renderTarget (Left names) = T.intercalate " / " names
    renderTarget (Right n)    = T.pack (show n)

-- | Renderiza la clave en formato Indentado clásico con sangría jerárquica y numeración única.
renderIndentedKey :: KeyLang -> KeyTree -> Text
renderIndentedKey _ (KeyLeaf []) = ""
renderIndentedKey _ (KeyLeaf taxa) = T.intercalate " / " taxa <> "\n"
renderIndentedKey _ rootTree =
  T.unlines (snd (go 1 0 rootTree))
  where
    go :: Int -> Int -> KeyTree -> (Int, [Text])
    go nextNum _ (KeyLeaf taxa) =
      (nextNum, [T.intercalate " / " taxa])
    go myNum depth (KeyNode dec) =
      let indent = T.replicate (depth * 2) " "
          step (currNext, accLines) (leadIdx, br) =
            let leadPrefix = indent <> T.pack (show myNum) <> letterSuffix leadIdx <> ". "
                          <> formatLeadStatement (kdFeature dec) (kbStateLabel br)
            in case kbSubtree br of
                 KeyLeaf names ->
                   let line = leadPrefix <> " -> " <> T.intercalate " / " names
                   in (currNext, accLines ++ [line])
                 sub@(KeyNode _) ->
                   let (next', subLines) = go currNext (depth + 1) sub
                   in (next', accLines ++ [leadPrefix] ++ subLines)
      in foldl' step (myNum + 1, []) (zip [0 ..] (kdBranches dec))

    letterSuffix :: Int -> Text
    letterSuffix i =
      let c = toEnum (fromEnum 'a' + (i `mod` 26)) :: Char
      in T.singleton c

-- | Renderiza la clave en formato Markdown estructurado con dilemas numerados secuencialmente,
-- soportando inglés ('LangEn') y español ('LangEs').
renderMarkdownKey :: KeyLang -> KeyTree -> Text
renderMarkdownKey lang tree =
  let title = case lang of
        LangEn -> "# Identification Key"
        LangEs -> "# Clave de Identificación"
      subtitle = case lang of
        LangEn -> "_Generated by TAXIDE via Shannon Information Gain_"
        LangEs -> "_Generada por TAXIDE mediante Ganancia de Información de Shannon_"
      body = case tree of
        KeyLeaf []   -> []
        KeyLeaf taxa -> ["- **" <> T.intercalate " / " (map italicize taxa) <> "**"]
        KeyNode _    ->
          let (_, couplets) = numberCouplets 1 0 tree
          in concatMap renderMdCouplet couplets
  in T.unlines ([title, "", subtitle, ""] ++ body)
  where
    goToLabel = case lang of
      LangEn -> "go to couplet #"
      LangEs -> "ir al dilema #"

    parentLabel p
      | p <= 0    = ""
      | otherwise = " (" <> T.pack (show p) <> ")"

    italicize t = "*" <> t <> "*"

    renderMdCouplet :: (Int, Int, Text, [(Text, Either [Text] Int)]) -> [Text]
    renderMdCouplet (num, parentNum, _, leads) =
      [ let leadTag = if idx == (0 :: Int)
                        then T.pack (show num) <> parentLabel parentNum <> "."
                        else T.pack (show num) <> T.replicate idx "'" <> "."
            targetTxt = case target of
              Left names -> " $\\rightarrow$ **" <> T.intercalate " / " (map italicize names) <> "**"
              Right dst  -> " *(" <> goToLabel <> T.pack (show dst) <> ")*"
        in "- **" <> leadTag <> "** " <> leadTxt <> targetTxt
      | (idx, (leadTxt, target)) <- zip [0 ..] leads
      ] ++ [""]
