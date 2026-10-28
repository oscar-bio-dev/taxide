{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Engine.Fitch
Description : Parsimonia de Fitch en hardware O(1) sobre máscaras de bits Word64
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Implementa el algoritmo de máxima parsimonia de Fitch (1971) para caracteres discretos
desordenados (@UM@, @EUM@) mediante operaciones a nivel de bits (@.&.@, @.|.@, @popCount@)
sobre las columnas desempacadas @VU.Vector Word64@ de 'ColumnarMatrix'.
Incluye el pase post-orden de longitud y cotas ($s_k, m_k, g_k$) y el segundo pase
pre-orden de reconstrucción de estados ancestrales en nodos internos.
-}

module Taxide.Engine.Fitch
  ( FitchCharResult (..)
  , FitchTree (..)
  , fitchPrelimMask
  , fitchFinalMask
  , evaluateFitchCharacter
  , evaluateFitchColumn
  , reconstructFitchAncestors
  , fullCharacterMask
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (bit, complement, popCount, shiftL, testBit, (.&.), (.|.))
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import GHC.Generics (Generic)

import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Engine.Matrix
  ( ColumnarMatrix (..)
  , PseudoTag
  , maxBitsetStates
  , tagValid
  )
import Taxide.Engine.Tree (PhyloTree (..))

-- | Resultado de evaluar un carácter desordenado bajo parsimonia de Fitch.
data FitchCharResult = FitchCharResult
  { fcrSteps    :: !Int    -- ^ Longitud observada del carácter en el árbol: s_k = L_k(T)
  , fcrMinSteps :: !Int    -- ^ Cota inferior teórica libre de homoplasia: m_k
  , fcrMaxSteps :: !Int    -- ^ Cota superior en árbol estrella (politomía): g_k
  , fcrRootMask :: !Word64 -- ^ Conjunto preliminar de estados óptimos en la raíz
  } deriving (Eq, Show, Generic, NFData)

-- | Árbol anotado con los conjuntos de estados del primer pase (post-orden) y segundo pase
-- (pre-orden) de Fitch en cada hoja y nodo interno.
data FitchTree a
  = FitchLeaf !a !Word64 !Word64
  -- ^ Hoja: @(taxon, máscaraPreliminar, máscaraFinal)@
  | FitchInternal !Word64 !Word64 !Int !(FitchTree a) !(FitchTree a)
  -- ^ Nodo interno: @(máscaraPreliminar, máscaraFinal, costoLocal, hijoIzq, hijoDer)@
  deriving (Eq, Show, Generic, NFData)

-- | Obtiene la máscara preliminar (pase post-orden) de la raíz de un 'FitchTree'.
fitchPrelimMask :: FitchTree a -> Word64
fitchPrelimMask (FitchLeaf _ p _)         = p
fitchPrelimMask (FitchInternal p _ _ _ _) = p

-- | Obtiene la máscara ancestral final (pase pre-orden) de la raíz de un 'FitchTree'.
fitchFinalMask :: FitchTree a -> Word64
fitchFinalMask (FitchLeaf _ _ f)         = f
fitchFinalMask (FitchInternal _ f _ _ _) = f

-- | Construye la máscara completa de estados activos $\mathcal{S}_k$ para un carácter,
-- empleada en hojas con pseudo-valores @tagUnknown@, @tagInapplicable@ o @tagVariable@.
fullCharacterMask :: Int -> VU.Vector Word64 -> VU.Vector PseudoTag -> Word64
fullCharacterMask declaredStates col tags =
  let n = min (VU.length col) (VU.length tags)
      observedUnion = foldl' step 0 [0 .. n - 1]
      step acc i =
        if tags VU.! i == tagValid
          then acc .|. (col VU.! i)
          else acc
      declaredMask
        | declaredStates <= 0 = 0
        | declaredStates >= maxBitsetStates = complement 0
        | otherwise = (1 `shiftL` declaredStates) - 1
      combined = observedUnion .|. declaredMask
  in if combined == 0 then 1 else combined

-- | Evalúa un carácter desordenado (@cid@) de la 'ColumnarMatrix' sobre un árbol filogenético.
evaluateFitchCharacter
  :: SpecsDef
  -> ColumnarMatrix
  -> Int
  -> PhyloTree Int
  -> FitchCharResult
evaluateFitchCharacter specs mat cid tree =
  let mDecl = Map.findWithDefault (specMaxNumberStates specs) cid (specNumbersOfStates specs)
  in case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
       (Just col, Just tags) -> evaluateFitchColumn mDecl col tags tree
       _                     -> FitchCharResult 0 0 0 0

-- | Evalúa directamente los vectores @VU.Vector Word64@ y @VU.Vector PseudoTag@ de una
-- columna discreta sobre un árbol filogenético en $O(1)$ operaciones de palabra por nodo.
evaluateFitchColumn
  :: Int
  -> VU.Vector Word64
  -> VU.Vector PseudoTag
  -> PhyloTree Int
  -> FitchCharResult
evaluateFitchColumn declaredStates col tags tree =
  let !allMask = fullCharacterMask declaredStates col tags
      !nTaxa   = min (VU.length col) (VU.length tags)

      -- Hojas válidas con al menos un bit activo
      validMasks =
        [ m
        | i <- [0 .. nTaxa - 1]
        , tags VU.! i == tagValid
        , let m = col VU.! i
        , m /= 0
        ]

      (!rootMask, !steps) = postOrder allMask tree
      !minSteps = computeMinStepsFitch validMasks
      !maxSteps = computeMaxStepsFitch validMasks
  in FitchCharResult
       { fcrSteps    = if null validMasks then 0 else steps
       , fcrMinSteps = minSteps
       , fcrMaxSteps = maxSteps
       , fcrRootMask = rootMask
       }
  where
    nLen = min (VU.length col) (VU.length tags)

    leafMask :: Word64 -> Int -> Word64
    leafMask !allMask !tIdx
      | tIdx >= 0 && tIdx < nLen =
          let !tag = tags VU.! tIdx
              !msk = col VU.! tIdx
          in if tag == tagValid && msk /= 0
               then msk
               else allMask
      | otherwise = allMask

    postOrder :: Word64 -> PhyloTree Int -> (Word64, Int)
    postOrder !allMask (PhyloLeaf tIdx _) =
      (leafMask allMask tIdx, 0)
    postOrder !allMask (PhyloNode l r _) =
      let (!sL, !cL) = postOrder allMask l
          (!sR, !cR) = postOrder allMask r
          !inter     = sL .&. sR
      in if inter /= 0
           then (inter, cL + cR)
           else (sL .|. sR, cL + cR + 1)

-- | Calcula el mínimo cambio teórico posible $m_k$ para un carácter desordenado.
--
-- Cuando todas las hojas válidas tienen un único estado, $m_k = |\bigcup S_u| - 1$.
-- En presencia de hojas polimórficas, determina el tamaño mínimo de un conjunto
-- transversal de estados que intersecte todas las hojas válidas menos 1.
computeMinStepsFitch :: [Word64] -> Int
computeMinStepsFitch [] = 0
computeMinStepsFitch validMasks =
  let singletons = [m | m <- validMasks, popCount m == 1]
      reqMask    = foldl' (.|.) 0 singletons
      uncovered  = [m | m <- validMasks, (m .&. reqMask) == 0]
      totalStates = greedyCover reqMask uncovered
  in max 0 (popCount totalStates - 1)
  where
    greedyCover :: Word64 -> [Word64] -> Word64
    greedyCover !chosen [] = chosen
    greedyCover !chosen remaining =
      let allBitsUnion = foldl' (.|.) 0 remaining
          candidateBits = [b | b <- [0 .. maxBitsetStates - 1], testBit allBitsUnion b]
          bestBit = snd $ maximum
            [ (length [m | m <- remaining, testBit m b], b)
            | b <- candidateBits
            ]
          chosen' = chosen .|. bit bestBit
          remaining' = [m | m <- remaining, (m .&. chosen') == 0]
      in greedyCover chosen' remaining'

-- | Calcula el máximo cambio posible $g_k$ sobre un árbol estrella (politomía total):
-- $g_k = N_{\text{válidas}} - \max_{a} |\{u : a \in S_u\}|$.
computeMaxStepsFitch :: [Word64] -> Int
computeMaxStepsFitch [] = 0
computeMaxStepsFitch validMasks =
  let nValid = length validMasks
      unionMask = foldl' (.|.) 0 validMasks
      activeStates = [b | b <- [0 .. maxBitsetStates - 1], testBit unionMask b]
      maxFreq = case activeStates of
        [] -> 0
        bs -> maximum [length [m | m <- validMasks, testBit m b] | b <- bs]
  in max 0 (nValid - maxFreq)

-- | Ejecuta los dos pases de Fitch (post-orden y pre-orden) para reconstruir los
-- conjuntos de estados ancestrales óptimos en todos los nodos internos y hojas del árbol.
reconstructFitchAncestors
  :: Int
  -> VU.Vector Word64
  -> VU.Vector PseudoTag
  -> PhyloTree Int
  -> FitchTree Int
reconstructFitchAncestors declaredStates col tags tree =
  let !allMask = fullCharacterMask declaredStates col tags
      prelimTree = buildPrelim allMask tree
      !rootPrelim = fitchPrelimMask prelimTree
  in refinePreOrder rootPrelim prelimTree
  where
    nLen = min (VU.length col) (VU.length tags)

    leafMask !allMask !tIdx
      | tIdx >= 0 && tIdx < nLen =
          let !tag = tags VU.! tIdx
              !msk = col VU.! tIdx
          in if tag == tagValid && msk /= 0
               then msk
               else allMask
      | otherwise = allMask

    buildPrelim :: Word64 -> PhyloTree Int -> FitchTree Int
    buildPrelim !allMask (PhyloLeaf tIdx _) =
      let !m = leafMask allMask tIdx
      in FitchLeaf tIdx m m
    buildPrelim !allMask (PhyloNode l r _) =
      let l' = buildPrelim allMask l
          r' = buildPrelim allMask r
          !sL = fitchPrelimMask l'
          !sR = fitchPrelimMask r'
          !inter = sL .&. sR
          (!sA, !cost) = if inter /= 0 then (inter, 0) else (sL .|. sR, 1)
      in FitchInternal sA sA cost l' r'

    refinePreOrder :: Word64 -> FitchTree Int -> FitchTree Int
    refinePreOrder !parentFinal (FitchLeaf tIdx sV _) =
      let !inter = sV .&. parentFinal
          !finalMask = if inter /= 0 then inter else sV
      in FitchLeaf tIdx sV finalMask
    refinePreOrder !parentFinal (FitchInternal sV _ cost l r) =
      let !sL = fitchPrelimMask l
          !sR = fitchPrelimMask r
          !interParent = sV .&. parentFinal
          !finalV
            | interParent == parentFinal = parentFinal
            | (sL .&. sR) == 0           = sV .&. parentFinal
            | otherwise                  = sV .|. (parentFinal .&. (sL .|. sR))
          !finalClean = if finalV /= 0 then finalV else sV
      in FitchInternal sV finalClean cost (refinePreOrder finalClean l) (refinePreOrder finalClean r)
