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
1. Selección recursiva en cada nodo de decisión del carácter que maximiza la ganancia
   de información de Shannon penalizada por duplicación polimórfica y costo de
   observación $\mathrm{Score}(c; \mathcal{T}')$.
2. Propagación completa de taxones polimórficos o con incertidumbre hacia todas sus
   ramas compatibles, garantizando que el 100% de los taxones sean alcanzables.
3. Renderizadores puros hacia Clave Pareada / Bracketed, Clave Indentada y Markdown,
   soportando inglés ('LangEn') y español ('LangEs').
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

    -- * Renderizadores Puros
  , renderBracketedKey
  , renderIndentedKey
  , renderMarkdownKey
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (bit, shiftL, testBit, (.|.))
import Data.List (maximumBy, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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
  , DiscretizedCharacter (..)
  , discretizeTaxaValues
  )
import Taxide.Engine.Matrix
  ( ColumnarMatrix (..)
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
  -- ^ Costos empíricos de observación $w_c > 0$ por carácter (por defecto $1.0$)
  } deriving (Eq, Show, Generic, NFData)

-- | Opciones por defecto: regla de la raíz cuadrada, valor central y pesos uniformes $w_c = 1.0$.
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
  -- ^ Bifurcación o ramificación basada en los estados de un carácter
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

-- | Representación interna de un carácter candidato preparado para particionar taxones.
data CandidateChar = CandidateChar
  { ccId         :: !Int
  , ccFeature    :: !Text
  , ccWeight     :: !Double
  , ccStates     :: ![(StateId, Text)]
  , ccTaxonMasks :: !(VU.Vector Word64)
  }

-- | Construye recursivamente la clave de identificación seleccionando en cada paso el
-- carácter que maximiza $\mathrm{Score}(c; \mathcal{T}')$ (Ecuación 5.17).
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
      usedChars = Set.empty
  in if nTaxa <= 0
       then KeyLeaf []
       else buildNode candidates usedChars allTaxaIndices
  where
    taxonName :: Int -> Text
    taxonName idx
      | idx >= 0 && idx < V.length (cmTaxonNames mat) = cmTaxonNames mat V.! idx
      | otherwise = "Taxon_" <> T.pack (show idx)

    buildNode :: [CandidateChar] -> Set Int -> [Int] -> KeyTree
    buildNode _ _ [] = KeyLeaf []
    buildNode _ _ [singleIdx] = KeyLeaf [taxonName singleIdx]
    buildNode candidates used subset =
      let available = [c | c <- candidates, Set.notMember (ccId c) used]
          scored =
            [ (score, gain, c, activeBranches)
            | c <- available
            , Just (gain, score, activeBranches) <- [evaluateCandidate c subset]
            ]
      in case scored of
           [] ->
             -- Ningún carácter restante puede subdividir este subconjunto
             KeyLeaf (map taxonName subset)
           _ ->
             let (bestScore, bestGain, bestChar, bestBranches) =
                   maximumBy (comparing (\(s, g, c, _) -> (s, g, negate (ccId c)))) scored
                 used' = Set.insert (ccId bestChar) used
                 hBase = shannonEntropy (length subset)
                 childBranches =
                   [ KeyBranch
                       { kbStateId    = stId
                       , kbStateLabel = stLbl
                       , kbSubtree    = buildNode candidates used' subTaxa
                       }
                   | (stId, stLbl, subTaxa) <- bestBranches
                   ]
             in KeyNode KeyDecision
                  { kdCharId   = ccId bestChar
                  , kdFeature  = ccFeature bestChar
                  , kdEntropy  = hBase
                  , kdGain     = bestGain
                  , kdScore    = bestScore
                  , kdBranches = childBranches
                  }

-- | Evalúa un carácter candidato sobre el subconjunto de taxones $\mathcal{T}'$,
-- verificando completitud ($\bigcup_s \mathcal{T}'_s = \mathcal{T}'$) y reducción
-- estricta ($0 < |\mathcal{T}'_s| < |\mathcal{T}'|$), y calculando $\mathrm{Gain}$ y $\mathrm{Score}$.
evaluateCandidate
  :: CandidateChar
  -> [Int]
  -> Maybe (Double, Double, [(StateId, Text, [Int])])
evaluateCandidate cand subset =
  let nSub = length subset
      branches =
        [ (stId, stLbl, compatibleTaxa)
        | (stId, stLbl) <- ccStates cand
        , let bitIdx = fromIntegral stId - 1
        , let compatibleTaxa =
                [ tIdx
                | tIdx <- subset
                , tIdx >= 0 && tIdx < VU.length (ccTaxonMasks cand)
                , testBit (ccTaxonMasks cand VU.! tIdx) bitIdx
                ]
        , not (null compatibleTaxa)
        ]
      coveredSet = Set.fromList (concat [ts | (_, _, ts) <- branches])
      subsetSet  = Set.fromList subset
      allStrictSubsets = all (\(_, _, ts) -> length ts < nSub) branches
  in if length branches < 2 || coveredSet /= subsetSet || not allStrictSubsets
       then Nothing
       else
         let !hBase = shannonEntropy nSub
             !totalBranchSizes = sum [length ts | (_, _, ts) <- branches]
             !totalD = fromIntegral totalBranchSizes :: Double
             !expectedChildEntropy = sum
               [ let !sz = length ts
                     !szD = fromIntegral sz :: Double
                 in (szD / totalD) * shannonEntropy sz
               | (_, _, ts) <- branches
               ]
             !gain = max 1.0e-9 (hBase - expectedChildEntropy)
             !dupPenalty = totalD / fromIntegral nSub
             !w = max 1.0e-6 (ccWeight cand)
             !score = gain / (w * dupPenalty)
         in Just (gain, score, branches)

-- | Prepara todos los caracteres discretos y cuantitativos discretizados para la clave.
prepareCandidateChars
  :: KeyOptions
  -> SpecsDef
  -> [CharacterDef]
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> [CandidateChar]
prepareCandidateChars opts specs chars taxa mat =
  let charMap = Map.fromList [(charNumber c, c) | c <- chars]
  in concatMap (buildCand charMap) [1 .. cmNumChars mat]
  where
    nTaxa = cmNumTaxa mat
    typeMap = cmCharTypes mat

    weightOf cid =
      case Map.lookup cid (koCharWeights opts) of
        Just w | w > 0.0 -> w
        _                -> 1.0

    featureOf charMap cid =
      case Map.lookup cid charMap of
        Just c | not (T.null (T.strip (charFeature c))) -> T.strip (charFeature c)
        _ -> "character " <> T.pack (show cid)

    stateLabelOf charMap cid stId =
      case Map.lookup cid charMap >>= lookup stId . charStates of
        Just lbl | not (T.null (T.strip lbl)) -> T.strip lbl
        _ -> "state " <> T.pack (show stId)

    unitsOf charMap cid =
      case Map.lookup cid charMap >>= charUnits of
        Just u | not (T.null (T.strip u)) -> " " <> T.strip u
        _ -> ""

    buildCand charMap cid =
      case Map.findWithDefault UM cid typeMap of
        TE  -> []
        IN  -> buildQuantitative charMap cid
        RN  -> buildQuantitative charMap cid
        _   -> buildDiscrete charMap cid

    buildDiscrete charMap cid =
      case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
        (Just col, Just tags) ->
          let mDecl = Map.findWithDefault 0 cid (specNumbersOfStates specs)
              fromChars = case Map.lookup cid charMap of
                Just c -> maximum (0 : map (fromIntegral . fst) (charStates c))
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
          in [ CandidateChar
                 { ccId         = cid
                 , ccFeature    = featureOf charMap cid
                 , ccWeight     = weightOf cid
                 , ccStates     = statesList
                 , ccTaxonMasks = masks
                 }
             ]
        _ -> []

    complementZero :: Word64
    complementZero = maxBound

    buildQuantitative charMap cid =
      let dc = discretizeTaxaValues cid (koDiscretizeMethod opts) (koDiscretizeTarget opts) taxa
          k  = dcNumClasses dc
          bounds = dcBoundaries dc
          units = unitsOf charMap cid
      in if k < 2 || length bounds < k + 1
           then []
           else
             let !fullMask = (1 `shiftL` k) - 1
                 statesList =
                   [ let lo = bounds !! (s - 1)
                         hi = bounds !! s
                         lbl = fmtDouble lo <> "–" <> fmtDouble hi <> units
                     in (fromIntegral s, lbl)
                   | s <- [1 .. k]
                   ]
                 taxonIds =
                   if length taxa == nTaxa && not (null taxa)
                     then map rtIndex taxa
                     else [1 .. nTaxa]
                 masks = VU.fromList
                   [ case Map.lookup tKey (dcAssignments dc) of
                       Just s | s >= 1 && fromIntegral s <= maxBitsetStates ->
                         bit (fromIntegral s - 1)
                       _ -> fullMask
                   | tKey <- taxonIds
                   ]
             in [ CandidateChar
                    { ccId         = cid
                    , ccFeature    = featureOf charMap cid
                    , ccWeight     = weightOf cid
                    , ccStates     = statesList
                    , ccTaxonMasks = masks
                    }
                ]

fmtDouble :: Double -> Text
fmtDouble d
  | d == fromIntegral (round d :: Integer) = T.pack (show (round d :: Integer))
  | otherwise =
      let raw = T.pack (showFFloat (Just 2) d "")
          trimmed = T.dropWhileEnd (== '0') raw
      in if T.isSuffixOf "." trimmed then T.dropEnd 1 trimmed else trimmed

--------------------------------------------------------------------------------
-- Renderizadores Puros: Bracketed, Indented y Markdown
--------------------------------------------------------------------------------

-- | Renderiza la clave en formato clásico Pareado / Bracketed (numeración @1(0). ...@).
renderBracketedKey :: KeyLang -> KeyTree -> Text
renderBracketedKey _ (KeyLeaf []) = ""
renderBracketedKey _ (KeyLeaf taxa) = T.intercalate " / " taxa <> "\n"
renderBracketedKey _ rootNode@(KeyNode _) =
  let (_, couplets) = numberCouplets 1 0 rootNode
  in T.unlines (concatMap renderCouplet couplets)
  where
    -- Asigna números de dilema consecutivos en pre-orden
    numberCouplets :: Int -> Int -> KeyTree -> (Int, [(Int, Int, Text, [(Text, Either [Text] Int)])])
    numberCouplets nextNum _ (KeyLeaf _) = (nextNum, [])
    numberCouplets myNum parentNum (KeyNode dec) =
      let (nextAfterChildren, revLeads, childCoupletsList) =
            foldl'
              (\(currNext, accLeads, accCouplets) br ->
                let leadText = kdFeature dec <> " " <> kbStateLabel br
                in case kbSubtree br of
                     KeyLeaf names ->
                       (currNext, (leadText, Left names) : accLeads, accCouplets)
                     sub@(KeyNode _) ->
                       let childNum = currNext
                           (next', subCouplets) = numberCouplets (currNext + 1) myNum sub
                       in (next', (leadText, Right childNum) : accLeads, accCouplets ++ subCouplets)
              )
              (myNum + 1, [], [])
              (kdBranches dec)
          thisCouplet = (myNum, parentNum, kdFeature dec, reverse revLeads)
      in (nextAfterChildren, thisCouplet : childCoupletsList)

    renderCouplet :: (Int, Int, Text, [(Text, Either [Text] Int)]) -> [Text]
    renderCouplet (num, parentNum, _, leads) =
      let prefixFirst = T.pack (show num) <> "(" <> T.pack (show parentNum) <> "). "
          prefixOther = T.pack (show num) <> "." <> T.replicate (T.length (T.pack (show parentNum)) + 2) " " <> " "
      in [ (if idx == (0 :: Int) then prefixFirst else prefixOther)
           <> leadTxt <> " ... " <> renderTarget target
         | (idx, (leadTxt, target)) <- zip [0 ..] leads
         ]

    renderTarget (Left names) = T.intercalate " / " names
    renderTarget (Right n)    = T.pack (show n)

-- | Renderiza la clave en formato Indentado clásico con sangría jerárquica.
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
                          <> kdFeature dec <> " " <> kbStateLabel br
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

-- | Renderiza la clave en formato Markdown estructurado, soportando inglés ('LangEn')
-- y español ('LangEs').
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
        KeyNode _    -> renderMdNode 0 1 tree
  in T.unlines ([title, "", subtitle, ""] ++ body)
  where
    goToLabel = case lang of
      LangEn -> "go to"
      LangEs -> "ir a"

    italicize t = "*" <> t <> "*"

    renderMdNode :: Int -> Int -> KeyTree -> [Text]
    renderMdNode _ _ (KeyLeaf taxa) =
      ["- **" <> T.intercalate " / " (map italicize taxa) <> "**"]
    renderMdNode depth stepNum (KeyNode dec) =
      let indent = T.replicate (depth * 2) " "
      in concatMap (renderBranch indent stepNum (kdFeature dec)) (zip [1 :: Int ..] (kdBranches dec))

    renderBranch :: Text -> Int -> Text -> (Int, KeyBranch) -> [Text]
    renderBranch indent stepNum feat (branchIdx, br) =
      let bullet = indent <> "- **" <> T.pack (show stepNum) <> "." <> T.pack (show branchIdx)
                <> "** " <> feat <> ": " <> kbStateLabel br
      in case kbSubtree br of
           KeyLeaf names ->
             [bullet <> " $\\rightarrow$ **" <> T.intercalate " / " (map italicize names) <> "**"]
           sub@(KeyNode subDec) ->
             (bullet <> " (" <> goToLabel <> " character #" <> T.pack (show (kdCharId subDec)) <> ")")
             : renderMdNode (depthFromIndent indent + 1) (stepNum + 1) sub

    depthFromIndent :: Text -> Int
    depthFromIndent t = fromMaybe 0 (Just (T.length t `div` 2))
