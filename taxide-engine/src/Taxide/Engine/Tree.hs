{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

{- |
Module      : Taxide.Engine.Tree
Description : Árboles filogenéticos bifurcados, serializadores (Newick/ASCII) y operadores topológicos
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Define el tipo de datos algebraico estricto 'PhyloTree' para representar cladogramas
y fenogramas con longitudes de rama opcionales, junto con los renderizadores a formato
estándar Newick y arte ASCII/Unicode, y los operadores estructurales puros de inserción,
poda y reconexión que sustentan la adición secuencial de Wagner, SPR y TBR.
-}

module Taxide.Engine.Tree
  ( -- * ADT de Árbol Filogenético
    PhyloTree (..)
  , mkLeaf
  , mkLeafWithBranch
  , mkNode
  , mkNodeWithBranch
  , treeBranchLength
  , setBranchLength
  , clearBranchLengths

    -- * Consultas y Proyecciones
  , treeLeaves
  , numLeaves
  , mapTreeTaxa

    -- * Renderizadores (Newick y ASCII/Unicode)
  , renderNewick
  , renderAsciiTree

    -- * Operadores Estructurales para Búsqueda Topológica (Wagner, SPR, TBR)
  , insertLeafEverywhere
  , regraftSubtreeEverywhere
  , pruneAllSubtrees
  , rerootSubtreeAll
  , sprNeighbors
  , tbrNeighbors
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Numeric (showFFloat)

-- | Árbol filogenético bifurcado con raíz y longitudes de rama opcionales.
data PhyloTree a
  = PhyloLeaf !a !(Maybe Double)
  -- ^ Hoja terminal (índice o nombre de taxón) y longitud de su rama incidente
  | PhyloNode !(PhyloTree a) !(PhyloTree a) !(Maybe Double)
  -- ^ Clado interno bifurcado con subárbol izquierdo, derecho y longitud de rama incidente
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, NFData, Hashable)

-- | Construye una hoja sin longitud de rama explícita.
mkLeaf :: a -> PhyloTree a
mkLeaf x = PhyloLeaf x Nothing

-- | Construye una hoja con longitud de rama explícita.
mkLeafWithBranch :: a -> Double -> PhyloTree a
mkLeafWithBranch x b = PhyloLeaf x (Just b)

-- | Construye un nodo interno sin longitud de rama explícita.
mkNode :: PhyloTree a -> PhyloTree a -> PhyloTree a
mkNode l r = PhyloNode l r Nothing

-- | Construye un nodo interno con longitud de rama explícita.
mkNodeWithBranch :: PhyloTree a -> PhyloTree a -> Double -> PhyloTree a
mkNodeWithBranch l r b = PhyloNode l r (Just b)

-- | Obtiene la longitud de rama incidente al nodo raíz del subárbol.
treeBranchLength :: PhyloTree a -> Maybe Double
treeBranchLength (PhyloLeaf _ mb)   = mb
treeBranchLength (PhyloNode _ _ mb) = mb

-- | Establece la longitud de rama incidente al nodo raíz del subárbol.
setBranchLength :: Maybe Double -> PhyloTree a -> PhyloTree a
setBranchLength mb (PhyloLeaf x _)   = PhyloLeaf x mb
setBranchLength mb (PhyloNode l r _) = PhyloNode l r mb

-- | Elimina todas las longitudes de rama de un árbol.
clearBranchLengths :: PhyloTree a -> PhyloTree a
clearBranchLengths (PhyloLeaf x _)   = PhyloLeaf x Nothing
clearBranchLengths (PhyloNode l r _) = PhyloNode (clearBranchLengths l) (clearBranchLengths r) Nothing

-- | Devuelve la lista de hojas del árbol en orden de izquierda a derecha.
treeLeaves :: PhyloTree a -> [a]
treeLeaves (PhyloLeaf x _)   = [x]
treeLeaves (PhyloNode l r _) = treeLeaves l ++ treeLeaves r

-- | Devuelve el número total de hojas terminales en el árbol.
numLeaves :: PhyloTree a -> Int
numLeaves (PhyloLeaf _ _)   = 1
numLeaves (PhyloNode l r _) = numLeaves l + numLeaves r

-- | Proyecta un árbol indexado por enteros (0..N-1) a los nombres de los taxones.
mapTreeTaxa :: Vector Text -> PhyloTree Int -> PhyloTree Text
mapTreeTaxa names = fmap lookupName
  where
    n = V.length names
    lookupName idx
      | idx >= 0 && idx < n = names V.! idx
      | otherwise           = "Taxon_" <> T.pack (show idx)

-- | Serializa un árbol filogenético al formato estándar Newick terminado en @;@.
renderNewick :: PhyloTree Text -> Text
renderNewick tree = go tree <> ";"
  where
    go (PhyloLeaf lbl mb) =
      sanitizeNewickLabel lbl <> renderBranch mb
    go (PhyloNode l r mb) =
      "(" <> go l <> "," <> go r <> ")" <> renderBranch mb

    renderBranch Nothing  = ""
    renderBranch (Just b) = ":" <> formatDouble b

-- | Limpia caracteres reservados de Newick en nombres de taxones.
sanitizeNewickLabel :: Text -> Text
sanitizeNewickLabel txt =
  let cleaned = T.map replaceReserved (T.strip txt)
  in if T.null cleaned then "Taxon" else cleaned
  where
    replaceReserved c
      | c `elem` ("(),:;" :: String) = '_'
      | c == ' '                     = '_'
      | otherwise                    = c

-- | Formatea un número de punto flotante con hasta 4 decimales, suprimiendo ceros superfluos.
formatDouble :: Double -> Text
formatDouble d =
  let raw = T.pack (showFFloat (Just 4) d "")
      trimmed = T.dropWhileEnd (== '0') raw
  in if T.isSuffixOf "." trimmed
       then trimmed <> "0"
       else trimmed

-- | Renderiza un árbol filogenético en arte ASCII/Unicode utilizando conectores
-- @├── @, @└── @ y @│   @.
renderAsciiTree :: PhyloTree Text -> Text
renderAsciiTree root =
  T.unlines (renderRoot root)
  where
    renderRoot (PhyloLeaf lbl mb) =
      [lbl <> formatAsciiBranch mb]
    renderRoot (PhyloNode l r mb) =
      let rootHeader = "+" <> formatAsciiBranch mb
      in rootHeader
         : renderSubtree "" False l
        ++ renderSubtree "" True  r

    renderSubtree prefix isLast (PhyloLeaf lbl mb) =
      let connector = if isLast then "└── " else "├── "
      in [prefix <> connector <> lbl <> formatAsciiBranch mb]
    renderSubtree prefix isLast (PhyloNode l r mb) =
      let connector   = if isLast then "└── " else "├── "
          childPrefix = prefix <> (if isLast then "    " else "│   ")
          nodeLabel   = "+" <> formatAsciiBranch mb
      in (prefix <> connector <> nodeLabel)
         : renderSubtree childPrefix False l
        ++ renderSubtree childPrefix True  r

    formatAsciiBranch Nothing  = ""
    formatAsciiBranch (Just b) = " [" <> formatDouble b <> "]"

-- | Inserta una nueva hoja @x@ en todas las aristas posibles de un árbol existente,
-- incluyendo la arista raíz. Para un árbol con @k@ hojas produce exactamente @2k - 1@ árboles.
insertLeafEverywhere :: a -> PhyloTree a -> [PhyloTree a]
insertLeafEverywhere x tree =
  regraftSubtreeEverywhere (PhyloLeaf x Nothing) tree

-- | Reinserta (injerta) un subárbol @pruned@ subdividiendo cualquier arista del árbol @residual@
-- (incluida su raíz).
regraftSubtreeEverywhere :: PhyloTree a -> PhyloTree a -> [PhyloTree a]
regraftSubtreeEverywhere pruned residual =
  let aboveRoot = PhyloNode residual pruned Nothing
      insideChildren = case residual of
        PhyloLeaf _ _ -> []
        PhyloNode l r mb ->
          [PhyloNode l' r mb | l' <- regraftSubtreeEverywhere pruned l]
          ++ [PhyloNode l r' mb | r' <- regraftSubtreeEverywhere pruned r]
  in aboveRoot : insideChildren

-- | Enumera todas las formas posibles de podar un subárbol propio @S@ de @T@,
-- devolviendo pares @(subarbolPodado, arbolResidual)@.
pruneAllSubtrees :: PhyloTree a -> [(PhyloTree a, PhyloTree a)]
pruneAllSubtrees (PhyloLeaf _ _) = []
pruneAllSubtrees (PhyloNode l r _) =
  let lClean = setBranchLength Nothing l
      rClean = setBranchLength Nothing r
      fromLeft  = [ (s, PhyloNode lRes rClean Nothing) | (s, lRes) <- pruneAllSubtrees lClean ]
      fromRight = [ (s, PhyloNode lClean rRes Nothing) | (s, rRes) <- pruneAllSubtrees rClean ]
  in (lClean, rClean) : (rClean, lClean) : (fromLeft ++ fromRight)

-- | Enumera todos los re-enraizados posibles de un subárbol @S@ al biseccionarlo
-- por cualquiera de sus aristas internas (operador requerido por TBR).
rerootSubtreeAll :: PhyloTree a -> [PhyloTree a]
rerootSubtreeAll tree =
  let clean = clearBranchLengths tree
  in case clean of
       PhyloLeaf _ _ -> [clean]
       PhyloNode l r _ ->
         clean : descend l r ++ descend r l
  where
    descend (PhyloLeaf _ _) _ = []
    descend (PhyloNode a b _) ctx =
      let rootAtA = PhyloNode a (PhyloNode b ctx Nothing) Nothing
          rootAtB = PhyloNode b (PhyloNode a ctx Nothing) Nothing
      in rootAtA
         : rootAtB
         : descend a (PhyloNode b ctx Nothing)
        ++ descend b (PhyloNode a ctx Nothing)

-- | Genera la vecindad SPR (*Subtree Pruning and Regrafting*) completa de un árbol.
sprNeighbors :: Eq a => PhyloTree a -> [PhyloTree a]
sprNeighbors tree =
  let clean = clearBranchLengths tree
  in [ candidate
     | (pruned, residual) <- pruneAllSubtrees clean
     , candidate <- regraftSubtreeEverywhere pruned residual
     , candidate /= clean
     ]

-- | Genera la vecindad TBR (*Tree Bisection and Reconnection*) completa de un árbol.
tbrNeighbors :: Eq a => PhyloTree a -> [PhyloTree a]
tbrNeighbors tree =
  let clean = clearBranchLengths tree
  in [ candidate
     | (pruned, residual) <- pruneAllSubtrees clean
     , rerooted <- rerootSubtreeAll pruned
     , candidate <- regraftSubtreeEverywhere rerooted residual
     , candidate /= clean
     ]

