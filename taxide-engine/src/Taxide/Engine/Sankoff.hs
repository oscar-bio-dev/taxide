{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Engine.Sankoff
Description : Parsimonia de Sankoff/Wagner mediante programación dinámica y evaluación global CI/RI/RC
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Implementa el algoritmo general de programación dinámica post-orden de Sankoff (1975)
y la parsimonia aditiva de Wagner de Farris (1970) sobre vectores desempacados
@VU.Vector Double@ para caracteres ordenados (@OM@, @EOM@) y caracteres cuantitativos
discretizados dinámicamente (@IN@, @RN@).
Asimismo, expone el evaluador unificado 'evaluateTreeParsimony' que despacha cada
carácter a Fitch o Sankoff y calcula los índices cladísticos globales $L(T)$, $CI$,
$RI$, $RC$ y $HI$ especificados en la Sección 5.4.
-}

module Taxide.Engine.Sankoff
  ( -- * Matrices de Costo de Transición
    StepMatrix (..)
  , stepCost
  , stepMatrixDim

    -- * Resultados por Carácter y Globales
  , SankoffCharResult (..)
  , CharMetrics (..)
  , TreeEvaluation (..)

    -- * Evaluación de Sankoff/Wagner
  , evaluateSankoffColumn
  , evaluateSankoffCharacter
  , discretizeNumericToColumn

    -- * Evaluador Unificado de Parsimonia
  , evaluateTreeParsimony
  , CompiledParsimonyMatrix
  , compileParsimonyMatrix
  , evaluateCompiledTree
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (bit, testBit, (.|.))
#if __GLASGOW_HASKELL__ < 910
import Data.List (foldl')
#endif
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import GHC.Generics (Generic)

import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Core.Types (CharacterType (..), StatRange (..))
import Taxide.Engine.Discretize
  ( DiscretizeMethod
  , DiscretizeTarget
  , DiscretizedCharacter (..)
  , discretizeQuantitative
  , discretizeTaxaValues
  )
import Taxide.Engine.Fitch (FitchCharResult (..), evaluateFitchColumn)
import Taxide.Engine.Matrix
  ( ColumnarMatrix (..)
  , NumericColumn (..)
  , PseudoTag
  , maxBitsetStates
  , tagUnknown
  , tagValid
  )
import Taxide.Engine.Tree (PhyloTree (..))
import Taxide.Engine.Variants (ResolvedTaxon (..))

-- | Infinito numérico seguro para programación dinámica sin desbordamiento en sumas.
sankoffInfinity :: Double
sankoffInfinity = 1.0e15

-- | Especificación de costos de transición $w(i, j)$ entre estados $i, j \in \{1, \dots, m\}$.
data StepMatrix
  = WagnerCost !Int
  -- ^ Parsimonia aditiva lineal de Wagner: $w(i, j) = |i - j|$ para $m$ estados
  | CustomStepMatrix !Int !(VU.Vector Double)
  -- ^ Matriz general $m \times m$ en orden row-major: $w(i, j) = \text{vec}[(i-1)m + (j-1)]$
  deriving (Eq, Show, Generic, NFData)

-- | Número de estados $m$ de la matriz de paso.
stepMatrixDim :: StepMatrix -> Int
stepMatrixDim (WagnerCost m)         = m
stepMatrixDim (CustomStepMatrix m _) = m

-- | Costo de transición $w(i, j)$ para estados $i, j \in \{1, \dots, m\}$ (indexados desde 1).
stepCost :: StepMatrix -> Int -> Int -> Double
stepCost (WagnerCost _) !i !j =
  fromIntegral (abs (i - j))
stepCost (CustomStepMatrix m vec) !i !j =
  let !idx = (i - 1) * m + (j - 1)
  in if idx >= 0 && idx < VU.length vec
       then vec VU.! idx
       else fromIntegral (abs (i - j))

-- | Resultado de evaluar un carácter ordenado mediante programación dinámica de Sankoff.
data SankoffCharResult = SankoffCharResult
  { scrSteps      :: !Double           -- ^ Longitud mínima en la raíz: $s_k = L_k(T)$
  , scrMinSteps   :: !Double           -- ^ Mínimo cambio teórico posible: $m_k$
  , scrMaxSteps   :: !Double           -- ^ Máximo cambio posible en árbol estrella: $g_k$
  , scrRootVector :: !(VU.Vector Double) -- ^ Vector de costos condicionales $C_{\text{raíz}}(i)$
  } deriving (Eq, Show, Generic, NFData)

-- | Métricas cladísticas individuales de un carácter evaluado sobre un árbol $T$.
data CharMetrics = CharMetrics
  { cmCharId      :: !Int
  , cmCharType    :: !CharacterType
  , cmSteps       :: !Double -- ^ Longitud observada $s_k = L_k(T)$
  , cmMinSteps    :: !Double -- ^ Cambio mínimo posible $m_k$
  , cmMaxSteps    :: !Double -- ^ Cambio máximo en árbol estrella $g_k$
  , cmConsistency :: !Double -- ^ Índice de consistencia individual $ci_k = m_k / s_k$
  , cmRetention   :: !Double -- ^ Índice de retención individual $ri_k = (g_k - s_k) / (g_k - m_k)$
  } deriving (Eq, Show, Generic, NFData)

-- | Evaluación cladística integral de un árbol filogenético $T$ sobre la matriz completa.
data TreeEvaluation = TreeEvaluation
  { teTotalLength      :: !Double       -- ^ Longitud total de parsimonia $L(T) = \sum s_k$
  , teTotalMinSteps    :: !Double       -- ^ Suma de mínimos teóricos $M = \sum m_k$
  , teTotalMaxSteps    :: !Double       -- ^ Suma de máximos en estrella $G = \sum g_k$
  , teConsistencyIndex :: !Double       -- ^ Índice de Consistencia global $CI(T)$
  , teRetentionIndex   :: !Double       -- ^ Índice de Retención global $RI(T)$
  , teRescaledCI       :: !Double       -- ^ Índice de Consistencia Reescalado $RC(T) = CI(T) \cdot RI(T)$
  , teHomoplasyIndex   :: !Double       -- ^ Índice de Homoplasia $HI(T) = 1 - CI(T)$
  , teCharMetrics      :: ![CharMetrics] -- ^ Desglose detallado por carácter activo
  } deriving (Eq, Show, Generic, NFData)

-- | Determina el número efectivo de estados $m \ge 1$ de una columna discreta.
effectiveStateCount :: Int -> VU.Vector Word64 -> VU.Vector PseudoTag -> Int
effectiveStateCount declared col tags =
  let n = min (VU.length col) (VU.length tags)
      unionMask = foldl' (\acc i -> if tags VU.! i == tagValid then acc .|. (col VU.! i) else acc) 0 [0 .. n - 1]
      maxObs = maximum (0 : [b + 1 | b <- [0 .. maxBitsetStates - 1], testBit unionMask b])
  in max 1 (max declared maxObs)

-- | Evalúa un carácter ordenado (@cid@) de la 'ColumnarMatrix' usando parsimonia de Wagner.
evaluateSankoffCharacter
  :: SpecsDef
  -> ColumnarMatrix
  -> Int
  -> PhyloTree Int
  -> SankoffCharResult
evaluateSankoffCharacter specs mat cid tree =
  let mDecl = Map.findWithDefault (specMaxNumberStates specs) cid (specNumbersOfStates specs)
  in case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
       (Just col, Just tags) ->
         let !m = effectiveStateCount mDecl col tags
         in evaluateSankoffColumn (WagnerCost m) col tags tree
       _ ->
         SankoffCharResult 0.0 0.0 0.0 VU.empty

-- | Evalúa una columna discreta bajo una 'StepMatrix' arbitraria mediante un catamorfismo
-- post-orden sobre @VU.Vector Double@ (Ecuación 5.5 de la especificación).
evaluateSankoffColumn
  :: StepMatrix
  -> VU.Vector Word64
  -> VU.Vector PseudoTag
  -> PhyloTree Int
  -> SankoffCharResult
evaluateSankoffColumn sm col tags tree =
  let !m = max 1 (stepMatrixDim sm)
      !nTaxa = min (VU.length col) (VU.length tags)
      validMasks =
        [ msk
        | i <- [0 .. nTaxa - 1]
        , tags VU.! i == tagValid
        , let msk = col VU.! i
        , msk /= 0
        ]
      !rootVec = postOrder m tree
      !steps   = if null validMasks || VU.null rootVec then 0.0 else VU.minimum rootVec
      !minSt   = computeMinStepsSankoff sm validMasks
      !maxSt   = computeMaxStepsSankoff sm validMasks
  in SankoffCharResult
       { scrSteps      = steps
       , scrMinSteps   = minSt
       , scrMaxSteps   = maxSt
       , scrRootVector = rootVec
       }
  where
    nLen = min (VU.length col) (VU.length tags)

    leafVector :: Int -> Int -> VU.Vector Double
    leafVector !m !tIdx
      | tIdx >= 0 && tIdx < nLen =
          let !tag = tags VU.! tIdx
              !msk = col VU.! tIdx
          in if tag == tagValid && msk /= 0
               then VU.generate m (\i -> if testBit msk i then 0.0 else sankoffInfinity)
               else VU.replicate m 0.0
      | otherwise = VU.replicate m 0.0

    postOrder :: Int -> PhyloTree Int -> VU.Vector Double
    postOrder !m (PhyloLeaf tIdx _) =
      leafVector m tIdx
    postOrder !m (PhyloNode l r _) =
      let !cL = postOrder m l
          !cR = postOrder m r
      in VU.generate m $ \i0 ->
           let !stI = i0 + 1
               !bestL = minTransition m cL stI
               !bestR = minTransition m cR stI
           in bestL + bestR

    minTransition :: Int -> VU.Vector Double -> Int -> Double
    minTransition !m !childVec !stI =
      let go !j !best
            | j > m     = best
            | otherwise =
                let !c = (childVec VU.! (j - 1)) + stepCost sm stI j
                in go (j + 1) (min best c)
      in go 1 sankoffInfinity

-- | Calcula el mínimo cambio teórico posible $m_k$ para un carácter ordenado.
--
-- Para parsimonia aditiva de Wagner, $m_k = \max(0, \max_u \min S_u - \min_u \max S_u)$.
computeMinStepsSankoff :: StepMatrix -> [Word64] -> Double
computeMinStepsSankoff _ [] = 0.0
computeMinStepsSankoff sm validMasks =
  let !m = max 1 (stepMatrixDim sm)
      bounds =
        [ (minimum active, maximum active)
        | msk <- validMasks
        , let active = [s | s <- [1 .. m], testBit msk (s - 1)]
        , not (null active)
        ]
  in case bounds of
       [] -> 0.0
       _  ->
         let maxMinState = maximum (map fst bounds)
             minMaxState = minimum (map snd bounds)
         in case sm of
              WagnerCost _ ->
                fromIntegral (max 0 (maxMinState - minMaxState))
              CustomStepMatrix _ _ ->
                if maxMinState <= minMaxState
                  then 0.0
                  else stepCost sm minMaxState maxMinState

-- | Calcula el máximo cambio posible $g_k$ sobre un árbol estrella:
-- $g_k = \min_{a \in \mathcal{S}} \sum_{u} \min_{b \in S_u} w(a, b)$.
computeMaxStepsSankoff :: StepMatrix -> [Word64] -> Double
computeMaxStepsSankoff _ [] = 0.0
computeMaxStepsSankoff sm validMasks =
  let !m = max 1 (stepMatrixDim sm)
      activePerLeaf =
        [ active
        | msk <- validMasks
        , let active = [s | s <- [1 .. m], testBit msk (s - 1)]
        , not (null active)
        ]
  in case activePerLeaf of
       [] -> 0.0
       leaves ->
         minimum
           [ sum [ minimum [stepCost sm a b | b <- leafStates] | leafStates <- leaves ]
           | a <- [1 .. m]
           ]

-- | Proyecta un carácter cuantitativo (@IN@ o @RN@) a una columna discreta ordenada (@OM@)
-- mediante 'Taxide.Engine.Discretize'.
discretizeNumericToColumn
  :: DiscretizeMethod
  -> DiscretizeTarget
  -> Int
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> Maybe (Int, VU.Vector Word64, VU.Vector PseudoTag)
discretizeNumericToColumn method target cid taxa mat =
  let nTaxa = cmNumTaxa mat
      dc = if not (null taxa)
             then discretizeTaxaValues cid method target taxa
             else case Map.lookup cid (cmNumericCols mat) of
                    Nothing -> DiscretizedCharacter cid 0 [] Map.empty
                    Just nc ->
                      let samples =
                            [ (tIdx, buildStatRange nc tIdx)
                            | tIdx <- [0 .. nTaxa - 1]
                            , (ncPseudoTags nc VU.! tIdx) == tagValid
                            ]
                      in discretizeQuantitative cid method target samples
      k = dcNumClasses dc
      assigns = dcAssignments dc
  in if k <= 0
       then Nothing
       else
         let taxonIds =
               if length taxa == nTaxa && not (null taxa)
                 then map rtIndex taxa
                 else [0 .. nTaxa - 1]
             cells =
               [ case Map.lookup tKey assigns of
                   Just s | s >= 1 && fromIntegral s <= maxBitsetStates ->
                     (bit (fromIntegral s - 1), tagValid)
                   _ ->
                     let origTag = case Map.lookup cid (cmNumericCols mat) of
                           Just nc | rowIdx < VU.length (ncPseudoTags nc) -> ncPseudoTags nc VU.! rowIdx
                           _ -> tagUnknown
                     in (0, if origTag == tagValid then tagUnknown else origTag)
               | (rowIdx, tKey) <- zip [0 ..] taxonIds
               ]
         in Just (k, VU.fromList (map fst cells), VU.fromList (map snd cells))
  where
    buildStatRange nc i = StatRange
      { srAbsoluteMin = Just (ncAbsMin nc VU.! i)
      , srNormalMin   = ncNormalLow nc VU.! i
      , srCenterVal   = Just (ncCenter nc VU.! i)
      , srNormalMax   = ncNormalHigh nc VU.! i
      , srAbsoluteMax = Just (ncAbsMax nc VU.! i)
      }

-- | Representación pre-compilada de las columnas de una matriz para evaluar múltiples
-- topologías en búsquedas heurísticas (Wagner, SPR, TBR) sin repetir la discretización.
data CompiledChar
  = CompiledFitch
      !Int                  -- ^ cid
      !CharacterType        -- ^ UM o EUM
      !Int                  -- ^ número de estados declarados
      !(VU.Vector Word64)   -- ^ máscaras
      !(VU.Vector PseudoTag) -- ^ etiquetas
  | CompiledSankoff
      !Int                  -- ^ cid
      !CharacterType        -- ^ OM, EOM, IN o RN
      !StepMatrix           -- ^ costos de paso
      !(VU.Vector Word64)   -- ^ máscaras
      !(VU.Vector PseudoTag) -- ^ etiquetas
  deriving (Eq, Show, Generic, NFData)

-- | Matriz pre-compilada lista para evaluación rápida de árboles candidatos.
data CompiledParsimonyMatrix = CompiledParsimonyMatrix ![CompiledChar]
  deriving (Eq, Show, Generic, NFData)

-- | Pre-compila todos los caracteres evaluables de una 'ColumnarMatrix', discretizando
-- una sola vez los caracteres cuantitativos (@IN@, @RN@).
compileParsimonyMatrix
  :: DiscretizeMethod
  -> DiscretizeTarget
  -> SpecsDef
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> CompiledParsimonyMatrix
compileParsimonyMatrix method target specs taxa mat =
  CompiledParsimonyMatrix (concatMap compileChar [1 .. cmNumChars mat])
  where
    typeMap = cmCharTypes mat

    compileChar cid =
      let cType = Map.findWithDefault UM cid typeMap
          mDecl = Map.findWithDefault (specMaxNumberStates specs) cid (specNumbersOfStates specs)
      in case cType of
           TE -> []
           UM ->
             case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
               (Just col, Just tags) -> [CompiledFitch cid UM mDecl col tags]
               _                     -> []
           EUM ->
             case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
               (Just col, Just tags) -> [CompiledFitch cid EUM mDecl col tags]
               _                     -> []
           OM ->
             case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
               (Just col, Just tags) ->
                 let !m = effectiveStateCount mDecl col tags
                 in [CompiledSankoff cid OM (WagnerCost m) col tags]
               _ -> []
           EOM ->
             case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
               (Just col, Just tags) ->
                 let !m = effectiveStateCount mDecl col tags
                 in [CompiledSankoff cid EOM (WagnerCost m) col tags]
               _ -> []
           IN ->
             case discretizeNumericToColumn method target cid taxa mat of
               Just (k, col, tags) -> [CompiledSankoff cid IN (WagnerCost k) col tags]
               Nothing             -> []
           RN ->
             case discretizeNumericToColumn method target cid taxa mat of
               Just (k, col, tags) -> [CompiledSankoff cid RN (WagnerCost k) col tags]
               Nothing             -> []

-- | Evalúa un árbol filogenético contra una 'CompiledParsimonyMatrix'.
evaluateCompiledTree :: CompiledParsimonyMatrix -> PhyloTree Int -> TreeEvaluation
evaluateCompiledTree (CompiledParsimonyMatrix compiledChars) tree =
  let charMetrics = map evalOne compiledChars
      !totalS = foldl' (\acc cm -> acc + cmSteps cm) 0.0 charMetrics
      !totalM = foldl' (\acc cm -> acc + cmMinSteps cm) 0.0 charMetrics
      !totalG = foldl' (\acc cm -> acc + cmMaxSteps cm) 0.0 charMetrics
      !ci = if totalS <= 0.0 then 1.0 else totalM / totalS
      !denomRI = totalG - totalM
      !ri = if denomRI <= 0.0 then 1.0 else (totalG - totalS) / denomRI
      !rc = ci * ri
      !hi = 1.0 - ci
  in TreeEvaluation
       { teTotalLength      = totalS
       , teTotalMinSteps    = totalM
       , teTotalMaxSteps    = totalG
       , teConsistencyIndex = ci
       , teRetentionIndex   = ri
       , teRescaledCI       = rc
       , teHomoplasyIndex   = hi
       , teCharMetrics      = charMetrics
       }
  where
    evalOne (CompiledFitch cid cType mDecl col tags) =
      let res = evaluateFitchColumn mDecl col tags tree
          !s  = fromIntegral (fcrSteps res)
          !m  = fromIntegral (fcrMinSteps res)
          !g  = fromIntegral (fcrMaxSteps res)
          !ci = if s <= 0.0 then 1.0 else m / s
          !ri = if g <= m then 1.0 else (g - s) / (g - m)
      in CharMetrics cid cType s m g ci ri

    evalOne (CompiledSankoff cid cType sm col tags) =
      let res = evaluateSankoffColumn sm col tags tree
          !s  = scrSteps res
          !m  = scrMinSteps res
          !g  = scrMaxSteps res
          !ci = if s <= 0.0 then 1.0 else m / s
          !ri = if g <= m then 1.0 else (g - s) / (g - m)
      in CharMetrics cid cType s m g ci ri

-- | Evaluador unificado de parsimonia: despacha cada carácter a Fitch (@UM@/@EUM@)
-- o Sankoff/Wagner (@OM@/@EOM@/@IN@/@RN@) y calcula las métricas globales $L(T)$,
-- $CI$, $RI$, $RC$ y $HI$, junto con el desglose por carácter.
evaluateTreeParsimony
  :: DiscretizeMethod
  -> DiscretizeTarget
  -> SpecsDef
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> PhyloTree Int
  -> TreeEvaluation
evaluateTreeParsimony method target specs taxa mat tree =
  let compiled = compileParsimonyMatrix method target specs taxa mat
  in evaluateCompiledTree compiled tree
