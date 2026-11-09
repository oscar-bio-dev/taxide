{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

{- |
Module      : Taxide.Engine.Tree
Description : Árboles filogenéticos bifurcados, jerarquía taxonómica superior, serializadores y operadores topológicos
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Define el tipo de datos algebraico estricto 'PhyloTree' para representar cladogramas
y fenogramas con longitudes de rama opcionales, junto con:
1. Extracción automática e inferencia de linajes taxonómicos superiores ('TaxonLineage':
   Filo, Clase, Subclase, Orden, Familia y Género) y renderizado de jerarquía taxonómica
   completa ('renderTaxonomicHierarchy').
2. Orientación filogenética por grupo externo ('orientPhylogeny'), escalonamiento canónico
   ('ladderizeTree') y renderizado de cladogramas anotados con etiquetas de clado en los
   nodos internos ('renderAnnotatedCladogram').
3. Serializadores a formato estándar Newick ('renderNewick') y arte ASCII/Unicode
   ('renderAsciiTree'), más operadores estructurales puros para Wagner, SPR y TBR.
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

    -- * Consultas, Proyecciones y Escalonamiento (Ladderization)
  , treeLeaves
  , numLeaves
  , mapTreeTaxa
  , ladderizeTree
  , orientPhylogeny

    -- * Jerarquía Taxonómica Superior y Cladogramas Anotados
  , TaxonLineage (..)
  , extractTaxonLineages
  , extractBinomialGenus
  , renderTaxonomicHierarchy
  , renderAnnotatedCladogram

    -- * Renderizadores (Newick y ASCII/Unicode)
  , renderNewick
  , renderAsciiTree

    -- * Operadores Estructurales para Búsqueda Topológica (Wagner, SPR, TBR)
  , insertLeafEverywhere
  , regraftSubtreeEverywhere
  , pruneAllSubtrees
  , rerootSubtreeAll
  , rerootWithBranchLengths
  , sprNeighbors
  , tbrNeighbors
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (testBit)
import Data.Char (isAlpha, isUpper)
import Data.Hashable (Hashable)
import Data.List (groupBy, maximumBy, nub, sortOn)
#if __GLASGOW_HASKELL__ < 910
import Data.List (foldl')
#endif
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Numeric (showFFloat)

import Taxide.Core.Character (CharacterDef (..))
import Taxide.Engine.Matrix (ColumnarMatrix (..), maxBitsetStates, tagValid)
import Taxide.Engine.Variants (ResolvedTaxon (..))

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

-- | Escalonamiento canónico (*ladderization*) de un árbol filogenético: ordena
-- recursivamente los hijos de cada nodo interno de modo que el subárbol con menor
-- número de hojas quede arriba (@├── @) y el clado más diverso continúe abajo (@└── @),
-- evitando escaleras diagonales de líneas verticales cruzadas.
ladderizeTree :: Ord a => PhyloTree a -> PhyloTree a
ladderizeTree leaf@(PhyloLeaf _ _) = leaf
ladderizeTree (PhyloNode l r mb) =
  let l' = ladderizeTree l
      r' = ladderizeTree r
      keySub s = (numLeaves s, minimum (treeLeaves s))
  in if keySub l' <= keySub r'
       then PhyloNode l' r' mb
       else PhyloNode r' l' mb

-- | Extrae el nombre genérico de un binomio taxonómico @"Género especie"@ (si el nombre
-- consta de al menos dos palabras y la primera comienza con mayúscula y contiene sólo letras).
extractBinomialGenus :: Text -> Maybe Text
extractBinomialGenus rawName =
  case T.words (T.strip rawName) of
    (firstWord : _ : _)
      | T.length firstWord >= 2
      , Just (c0, rest) <- T.uncons firstWord
      , isUpper c0
      , T.all isAlpha rest ->
          Just firstWord
    _ -> Nothing

--------------------------------------------------------------------------------
-- Linaje Taxonómico Superior (Filo -> Clase -> Subclase -> Orden -> Familia -> Género)
--------------------------------------------------------------------------------

-- | Linaje taxonómico jerárquico superior asociado a cada taxón del dataset.
data TaxonLineage = TaxonLineage
  { tlIndex     :: !Int
  , tlName      :: !Text
  , tlAuthority :: !(Maybe Text)
  , tlPhylum    :: !(Maybe Text) -- ^ Filo / División (ej. @"Annelida"@)
  , tlClass     :: !(Maybe Text) -- ^ Clase (ej. @"Clitellata"@)
  , tlSubclass  :: !(Maybe Text) -- ^ Subclase (ej. @"Oligochaeta"@)
  , tlOrder     :: !(Maybe Text) -- ^ Orden (ej. @"Crassiclitellata"@)
  , tlFamily    :: !(Maybe Text) -- ^ Familia (ej. @"Lumbricidae"@, @"Sparganophilidae"@)
  , tlGenus     :: !(Maybe Text) -- ^ Género binomial (ej. @"Aporrectodea"@, @"Bimastos"@)
  } deriving (Eq, Ord, Show, Generic, NFData)

-- | Extrae e infiere el linaje taxonómico superior ('TaxonLineage') de cada taxón
-- combinando:
-- 1. Caracteres de rango taxonómico definidos explícitamente en @CHARS@/@ITEMS@
--    (Filo, Clase, Subclase, Orden, Familia, Género).
-- 2. El nombre genérico del binomio científico (@Género especie@).
-- 3. La base de conocimiento sistemático para familias y géneros conocidos (Annelida
--    Clitellata/Oligochaeta según Glasby et al. 2025 / Reynolds 2017, Cnidaria, Tracheophyta, etc.).
extractTaxonLineages
  :: [CharacterDef]
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> Vector TaxonLineage
extractTaxonLineages chars taxa mat =
  let nTaxa = cmNumTaxa mat
      taxaVec = V.fromList taxa
  in V.generate nTaxa $ \idx ->
       let tName = if idx < V.length (cmTaxonNames mat)
                     then cmTaxonNames mat V.! idx
                     else "Taxon_" <> T.pack (show idx)
           mAuth = if idx < V.length taxaVec
                     then rtAuthority (taxaVec V.! idx)
                     else Nothing
           explicitPhylum   = lookupRankChar ["filo", "phylum", "división", "division"] [] idx
           explicitSubclass = lookupRankChar ["subclase", "subclass"] [] idx
           explicitClass    = lookupRankChar ["clase", "class"] ["subclase", "subclass"] idx
           explicitOrder    = lookupRankChar ["orden", "order"] ["suborden", "suborder"] idx
           explicitFamily   = lookupRankChar ["familia", "family"] ["subfamilia", "subfamily"] idx
           explicitGenus    = lookupRankChar ["género", "genero", "genus"] [] idx
           binomGenus       = extractBinomialGenus tName
           genus            = explicitGenus <|> binomGenus
           (kbPhylum, kbClass, kbSubclass, kbOrder, kbFamily) =
             inferHigherTaxonomy explicitFamily genus
       in TaxonLineage
            { tlIndex     = idx
            , tlName      = tName
            , tlAuthority = mAuth
            , tlPhylum    = explicitPhylum   <|> kbPhylum
            , tlClass     = explicitClass    <|> kbClass
            , tlSubclass  = explicitSubclass <|> kbSubclass
            , tlOrder     = explicitOrder    <|> kbOrder
            , tlFamily    = explicitFamily   <|> kbFamily
            , tlGenus     = genus
            }
  where
    Nothing <|> y = y
    x       <|> _ = x

    cleanComment txt =
      T.strip (T.takeWhile (/= '<') txt)

    lookupRankChar :: [Text] -> [Text] -> Int -> Maybe Text
    lookupRankChar posKeys negKeys rowIdx =
      listToMaybe
        [ cleanComment stLabel
        | cd <- chars
        , let featLower = T.toLower (charFeature cd)
        , any (`T.isInfixOf` featLower) posKeys
        , not (any (`T.isInfixOf` featLower) negKeys)
        , Just stLabel <- [lookupActiveStateLabel cd rowIdx]
        ]

    lookupActiveStateLabel :: CharacterDef -> Int -> Maybe Text
    lookupActiveStateLabel cd rowIdx =
      let cid = charNumber cd
      in case (Map.lookup cid (cmDiscreteCols mat), Map.lookup cid (cmDiscreteTags mat)) of
           (Just col, Just tags)
             | rowIdx >= 0 && rowIdx < VU.length col && rowIdx < VU.length tags
             , tags VU.! rowIdx == tagValid ->
                 let msk = col VU.! rowIdx
                     activeBits = [fromIntegral (b + 1) | b <- [0 .. maxBitsetStates - 1], testBit msk b]
                 in case activeBits of
                      (sId : _) -> lookup sId (charStates cd)
                      []        -> Nothing
           _ -> Nothing

-- | Base de conocimiento zoológico/botánico para completar rangos superiores (Filo, Clase,
-- Subclase, Orden, Familia) a partir de la familia o el género binomial cuando no están
-- codificados como columnas separadas en @CHARS@.
inferHigherTaxonomy
  :: Maybe Text
  -> Maybe Text
  -> (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
inferHigherTaxonomy mFam mGen =
  let fam = fromMaybe "" mFam
      gen = fromMaybe "" mGen
      crassiclitellataFamilies =
        [ "Lumbricidae", "Sparganophilidae", "Acanthodrilidae", "Megascolecidae"
        , "Ocnerodrilidae", "Eudrilidae", "Glossoscolecidae", "Hormogastridae"
        , "Criodrilidae", "Almidae", "Komarekionidae", "Lutodrilidae"
        ]
      lumbricidaeGenera =
        [ "Allolobophora", "Allolobophoridella", "Aporrectodea", "Bimastos"
        , "Dendrobaena", "Dendrodrilus", "Eisenia", "Eiseniella", "Eisenoides"
        , "Fitzingeria", "Helodrilus", "Lumbricus", "Murchieona", "Octodrilus"
        , "Octolasion", "Proctodrilus", "Satchellius"
        ]
      tubificidaFamilies =
        [ "Naididae", "Tubificidae", "Pristinidae", "Opistocystidae"
        , "Dorydrilidae", "Phreodrilidae", "Parvidrilidae"
        ]
      tubificidaGenera =
        [ "Tubifex", "Limnodrilus", "Aulodrilus", "Ilyodrilus", "Branchiura"
        , "Rhyacodrilus", "Bothrioneurum", "Potamothrix", "Psammoryctides"
        , "Spirosperma", "Quistadrilus", "Tasserkidrilus", "Nais", "Chaetogaster"
        , "Dero", "Pristina", "Stylaria", "Slavina", "Vejdovskyella", "Ophidonais"
        , "Uncinais", "Paranais", "Amphichaeta", "Specaria", "Piguetiella"
        , "Haemonais", "Arcteonais", "Ripistes", "Allonais", "Bratislavia"
        , "Stephensoniana", "Haber", "Varichaetadrilus"
        ]
      enchytraeidaGenera =
        [ "Enchytraeus", "Fridericia", "Lumbricillus", "Henlea", "Marionina"
        , "Cognettia", "Mesenchytraeus", "Achaeta", "Bryodrilus", "Cernosvitoviella"
        ]
      lumbriculidaGenera =
        [ "Lumbriculus", "Stylodrilus", "Eclipidrilus", "Rhynchelmis"
        , "Trichodrilus", "Kincaidiana", "Altmanella"
        ]
  in if fam `elem` crassiclitellataFamilies
       then (Just "Annelida", Just "Clitellata", Just "Oligochaeta", Just "Crassiclitellata", Just fam)
     else if gen `elem` lumbricidaeGenera
       then (Just "Annelida", Just "Clitellata", Just "Oligochaeta", Just "Crassiclitellata", Just "Lumbricidae")
     else if gen == "Sparganophilus"
       then (Just "Annelida", Just "Clitellata", Just "Oligochaeta", Just "Crassiclitellata", Just "Sparganophilidae")
     else if gen `elem` ["Diplocardia", "Microscolex", "Rhododrilus"]
       then (Just "Annelida", Just "Clitellata", Just "Oligochaeta", Just "Crassiclitellata", Just "Acanthodrilidae")
     else if gen `elem` ["Amynthas", "Metaphire", "Pheretima", "Pithemera", "Perionyx"]
       then (Just "Annelida", Just "Clitellata", Just "Oligochaeta", Just "Crassiclitellata", Just "Megascolecidae")
     else if gen == "Pontoscolex"
       then (Just "Annelida", Just "Clitellata", Just "Oligochaeta", Just "Crassiclitellata", Just "Glossoscolecidae")
     else if fam `elem` tubificidaFamilies || gen `elem` tubificidaGenera
       then (Just "Annelida", Just "Clitellata", Just "Oligochaeta", Just "Tubificida", Just (if T.null fam then "Naididae" else fam))
     else if fam == "Enchytraeidae" || gen `elem` enchytraeidaGenera
       then (Just "Annelida", Just "Clitellata", Just "Oligochaeta", Just "Enchytraeida", Just "Enchytraeidae")
     else if fam == "Lumbriculidae" || gen `elem` lumbriculidaGenera
       then (Just "Annelida", Just "Clitellata", Just "Oligochaeta", Just "Lumbriculida", Just "Lumbriculidae")
     else if gen == "Craspedacusta" || fam == "Olindiidae"
       then (Just "Cnidaria", Just "Hydrozoa", Nothing, Just "Limnomedusae", Just "Olindiidae")
     else if gen == "Quercus" || fam == "Fagaceae"
       then (Just "Tracheophyta", Just "Magnoliopsida", Nothing, Just "Fagales", Just "Fagaceae")
     else if gen `elem` ["Pinus", "Abies", "Picea"] || fam == "Pinaceae"
       then (Just "Tracheophyta", Just "Pinopsida", Nothing, Just "Pinales", Just "Pinaceae")
     else if gen == "Ephedra" || fam == "Ephedraceae"
       then (Just "Tracheophyta", Just "Gnetopsida", Nothing, Just "Ephedrales", Just "Ephedraceae")
     else (Nothing, Nothing, Nothing, Nothing, mFam)

-- | Nodo de un árbol jerárquico de clasificación taxonómica (Filo -> ... -> Especie).
data HierarchyTree
  = HierarchyRank !Text !Int ![HierarchyTree]
  | HierarchySpecies !Text

-- | Renderiza la clasificación jerárquica superior completa
-- (@Filo → Clase → Subclase → Orden → Familia → Género → Especie@) en arte Unicode.
renderTaxonomicHierarchy :: Vector TaxonLineage -> Text
renderTaxonomicHierarchy lineages
  | V.null lineages = ""
  | all (null . lineageRankPairs) (V.toList lineages) = ""
  | otherwise =
      let items =
            [ (lineageRankPairs tl, formatSpeciesWithAuth tl)
            | tl <- sortOn tlName (V.toList lineages)
            ]
          forest = buildHierarchyForest items
      in T.unlines (concatMap renderTopHierarchy forest)
  where
    lineageRankPairs :: TaxonLineage -> [Text]
    lineageRankPairs tl =
      catMaybes
        [ ("Filo " <>)     <$> tlPhylum tl
        , ("Clase " <>)    <$> tlClass tl
        , ("Subclase " <>) <$> tlSubclass tl
        , ("Orden " <>)    <$> tlOrder tl
        , ("Familia " <>)  <$> tlFamily tl
        , ("Género " <>)   <$> tlGenus tl
        ]

    formatSpeciesWithAuth :: TaxonLineage -> Text
    formatSpeciesWithAuth tl
      | "<" `T.isInfixOf` tlName tl = tlName tl
      | otherwise = tlName tl <> maybe "" (\a -> " <" <> a <> ">") (tlAuthority tl)

    buildHierarchyForest :: [([Text], Text)] -> [HierarchyTree]
    buildHierarchyForest entries =
      let (atLeaf, withRanks) =
            ( [sp | ([], sp) <- entries]
            , [(r, rs, sp) | (r : rs, sp) <- entries]
            )
          sortedRanks = sortOn (\(r, _, _) -> r) withRanks
          grouped     = groupBy (\(r1, _, _) (r2, _, _) -> r1 == r2) sortedRanks
          rankNodes   =
            [ let rLabel = case grp of ((r, _, _) : _) -> r; [] -> ""
                  subEntries = [(rs, sp) | (_, rs, sp) <- grp]
                  children   = buildHierarchyForest subEntries
              in HierarchyRank rLabel (length grp) children
            | grp <- grouped
            , not (null grp)
            ]
      in rankNodes ++ map HierarchySpecies atLeaf

    countLabel :: Int -> Text
    countLabel 1 = " (1 sp.)"
    countLabel n = " (" <> T.pack (show n) <> " spp.)"

    renderTopHierarchy :: HierarchyTree -> [Text]
    renderTopHierarchy (HierarchySpecies sp) = [sp]
    renderTopHierarchy (HierarchyRank lbl cnt children) =
      (lbl <> countLabel cnt) : renderChildren "" children

    renderChildren :: Text -> [HierarchyTree] -> [Text]
    renderChildren _ [] = []
    renderChildren prefix [lastChild] =
      renderSingleChild prefix True lastChild
    renderChildren prefix (c : cs) =
      renderSingleChild prefix False c ++ renderChildren prefix cs

    renderSingleChild :: Text -> Bool -> HierarchyTree -> [Text]
    renderSingleChild prefix isLast node =
      let conn   = if isLast then "└── " else "├── "
          nextPf = prefix <> (if isLast then "    " else "│   ")
      in case node of
           HierarchySpecies sp ->
             [prefix <> conn <> sp]
           HierarchyRank lbl cnt children ->
             (prefix <> conn <> lbl <> countLabel cnt)
               : renderChildren nextPf children

--------------------------------------------------------------------------------
-- Orientación por Grupo Externo (Outgroup Rooting) y Cladograma Anotado
--------------------------------------------------------------------------------

-- | Orienta un árbol filogenético indexado ($0 \dots N-1$) situando la raíz en la
-- arista del grupo externo taxonómico basal (por ejemplo, la familia o clase minoritaria)
-- si ello preserva o maximiza la monofilia de familias y géneros, y aplica 'ladderizeTree'.
orientPhylogeny :: Vector TaxonLineage -> PhyloTree Int -> PhyloTree Int
orientPhylogeny lineages tree
  | V.length lineages <= 2 = ladderizeTree tree
  | all (null . rankSignature) (V.toList lineages) = ladderizeTree tree
  | otherwise =
      let candidates = rerootWithBranchLengths tree
          outgroupSet = findBasalOutgroupSet lineages
          scoreCandidate cand =
            let isRootedAtOutgroup = case cand of
                  PhyloNode l r _ ->
                    not (Set.null outgroupSet)
                    && (Set.fromList (treeLeaves l) == outgroupSet
                        || Set.fromList (treeLeaves r) == outgroupSet)
                  _ -> False
                monoScore = countMonophyleticClades lineages cand
            in (isRootedAtOutgroup, monoScore)
          bestRooted =
            case candidates of
              [] -> tree
              _  -> maximumBy (comparing scoreCandidate) (reverse candidates)
      in ladderizeTree bestRooted
  where
    rankSignature tl =
      catMaybes [tlPhylum tl, tlClass tl, tlSubclass tl, tlOrder tl, tlFamily tl, tlGenus tl]

-- | Determina el conjunto de índices de taxones que conforman el grupo externo basal
-- (el estado minoritario del rango taxonómico más alto que presenta $\ge 2$ grupos).
findBasalOutgroupSet :: Vector TaxonLineage -> Set.Set Int
findBasalOutgroupSet lineages =
  let lins = V.toList lineages
      rankExtractors = [tlPhylum, tlClass, tlSubclass, tlOrder, tlFamily, tlGenus]
      findSplit [] = Set.empty
      findSplit (ext : rest) =
        let pairs = [( rVal, tlIndex tl ) | tl <- lins, Just rVal <- [ext tl]]
            sorted = sortOn fst pairs
            groups = groupBy (\a b -> fst a == fst b) sorted
        in if length groups >= 2 && sum (map length groups) == length lins
             then
               let nonEmptyGroups = [(length g, r0, map snd g) | g@((r0, _) : _) <- groups]
               in case sortOn (\(len, r0, _) -> (len, r0)) nonEmptyGroups of
                    ((_, _, idxs) : _) -> Set.fromList idxs
                    []                 -> Set.empty
             else findSplit rest
  in findSplit rankExtractors

-- | Cuenta el número de clados internos en el árbol que corresponden exactamente a una
-- familia o género monofilético completo.
countMonophyleticClades :: Vector TaxonLineage -> PhyloTree Int -> Int
countMonophyleticClades lineages root =
  let lins = V.toList lineages
      familySets =
        [ Set.fromList [tlIndex tl | tl <- lins, tlFamily tl == Just f]
        | f <- nub [f | tl <- lins, Just f <- [tlFamily tl]]
        ]
      genusSets =
        [ Set.fromList [tlIndex tl | tl <- lins, tlGenus tl == Just g]
        | g <- nub [g | tl <- lins, Just g <- [tlGenus tl]]
        ]
      targetSets = filter (\s -> Set.size s >= 2) (familySets ++ genusSets)
      cladeSets (PhyloLeaf i _) = [Set.singleton i]
      cladeSets (PhyloNode l r _) =
        let sL = cladeSets l
            sR = cladeSets r
            here = Set.fromList (treeLeaves l ++ treeLeaves r)
        in here : (sL ++ sR)
      allClades = Set.fromList (cladeSets root)
  in length [s | s <- targetSets, Set.member s allClades]

-- | Renderiza un cladograma o fenograma anotando cada nodo interno con el rango o clado
-- taxonómico compartido por sus hojas descendientes (Filo, Clase, Subclase, Orden, Familia,
-- Género o Subclado), suprimiendo los signos @+@ desnudos ambiguos.
renderAnnotatedCladogram :: Vector TaxonLineage -> PhyloTree Int -> Text
renderAnnotatedCladogram lineages rawTree =
  let oriented = orientPhylogeny lineages rawTree
  in T.unlines (renderRootNode oriented)
  where
    linsOfIndices :: [Int] -> [TaxonLineage]
    linsOfIndices idxs =
      [ lineages V.! i
      | i <- idxs
      , i >= 0 && i < V.length lineages
      ]

    sharedValue :: (TaxonLineage -> Maybe Text) -> [TaxonLineage] -> Maybe Text
    sharedValue _ [] = Nothing
    sharedValue ext (x : xs) =
      case ext x of
        Nothing -> Nothing
        Just v  -> if all (\y -> ext y == Just v) xs then Just v else Nothing

    sharedRankList :: [TaxonLineage] -> [(Text, Text)]
    sharedRankList lins =
      catMaybes
        [ (\v -> ("Filo", v))     <$> sharedValue tlPhylum lins
        , (\v -> ("Clase", v))    <$> sharedValue tlClass lins
        , (\v -> ("Subclase", v)) <$> sharedValue tlSubclass lins
        , (\v -> ("Orden", v))    <$> sharedValue tlOrder lins
        , (\v -> ("Familia", v))  <$> sharedValue tlFamily lins
        , (\v -> ("Género", v))   <$> sharedValue tlGenus lins
        ]

    formatRankBadges :: [(Text, Text)] -> Text
    formatRankBadges [] = ""
    formatRankBadges pairs =
      " [" <> T.intercalate " · " [rk <> " " <> val | (rk, val) <- pairs] <> "]"

    cladeAnnotation :: [(Text, Text)] -> PhyloTree Int -> ([(Text, Text)], Text)
    cladeAnnotation parentShared sub =
      let idxs = treeLeaves sub
          lins = linsOfIndices idxs
          currShared = sharedRankList lins
          newShared  = [p | p <- currShared, p `notElem` parentShared]
      in if not (null newShared)
           then (currShared, formatRankBadges newShared)
           else case lookup "Género" currShared of
                  Just g ->
                    (currShared, " [Subclado " <> g <> "]")
                  Nothing ->
                    let genera = nub [g | tl <- lins, Just g <- [tlGenus tl]]
                        fams   = nub [f | tl <- lins, Just f <- [tlFamily tl]]
                    in if length genera >= 2 && length genera <= 3
                         then (currShared, " [Clado " <> T.intercalate " + " genera <> "]")
                         else case fams of
                                [f] ->
                                  (currShared, " [Subclado " <> f <> " (" <> T.pack (show (length idxs)) <> " spp.)]")
                                _ ->
                                  (currShared, "")

    leafAnnotation :: [(Text, Text)] -> Int -> Text
    leafAnnotation parentShared idx
      | idx >= 0 && idx < V.length lineages =
          let tl = lineages V.! idx
              leafRanks = sharedRankList [tl]
              newLeafRanks =
                [ (rk, val)
                | (rk, val) <- leafRanks
                , rk `elem` ["Familia", "Género"]
                , (rk, val) `notElem` parentShared
                ]
              -- Si el nodo padre ya es el Género de esta especie, no repetimos [Género ...]
              filtered =
                case lookup "Género" parentShared of
                  Just _  -> filter (\(rk, _) -> rk /= "Género") newLeafRanks
                  Nothing -> newLeafRanks
          in formatRankBadges filtered
      | otherwise = ""

    leafName :: Int -> Text
    leafName idx
      | idx >= 0 && idx < V.length lineages = tlName (lineages V.! idx)
      | otherwise = "Taxon_" <> T.pack (show idx)

    renderRootNode :: PhyloTree Int -> [Text]
    renderRootNode (PhyloLeaf idx mb) =
      [leafName idx <> formatAsciiBranch mb <> leafAnnotation [] idx]
    renderRootNode root@(PhyloNode l r mb) =
      let (rootShared, rootBadge) = cladeAnnotation [] root
          rootHeader = "+" <> formatAsciiBranch mb <> rootBadge
      in rootHeader
         : renderSubtree "" False rootShared l
        ++ renderSubtree "" True  rootShared r

    renderSubtree :: Text -> Bool -> [(Text, Text)] -> PhyloTree Int -> [Text]
    renderSubtree prefix isLast parentShared (PhyloLeaf idx mb) =
      let connector = if isLast then "└── " else "├── "
          badge     = leafAnnotation parentShared idx
      in [prefix <> connector <> leafName idx <> formatAsciiBranch mb <> badge]
    renderSubtree prefix isLast parentShared node@(PhyloNode l r mb) =
      let connector   = if isLast then "└── " else "├── "
          childPrefix = prefix <> (if isLast then "    " else "│   ")
          (currShared, badge) = cladeAnnotation parentShared node
          nodeLabel   = "+" <> formatAsciiBranch mb <> badge
      in (prefix <> connector <> nodeLabel)
         : renderSubtree childPrefix False currShared l
        ++ renderSubtree childPrefix True  currShared r

--------------------------------------------------------------------------------
-- Serializadores Estándar (Newick y ASCII/Unicode)
--------------------------------------------------------------------------------

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

formatAsciiBranch :: Maybe Double -> Text
formatAsciiBranch Nothing  = ""
formatAsciiBranch (Just b) = " [" <> formatDouble b <> "]"

-- | Renderiza un árbol filogenético en arte ASCII/Unicode utilizando conectores
-- @├── @, @└── @ y @│   @, escalonando los clados ('ladderizeTree') y mostrando
-- etiquetas de género/familia en los nodos internos cuando las hojas tienen nombres
-- binomiales.
renderAsciiTree :: PhyloTree Text -> Text
renderAsciiTree rawRoot =
  let leaves = treeLeaves rawRoot
      hasBinomial = any (\t -> extractBinomialGenus t /= Nothing) leaves
  in if hasBinomial
       then
         let indexedVec = V.fromList (nub leaves)
             idxMap = Map.fromList (zip (V.toList indexedVec) [0 ..])
             idxTree = fmap (\t -> Map.findWithDefault 0 t idxMap) rawRoot
             lins = V.generate (V.length indexedVec) $ \i ->
               let nm = indexedVec V.! i
                   gen = extractBinomialGenus nm
                   (p, c, sc, o, f) = inferHigherTaxonomy Nothing gen
               in TaxonLineage i nm Nothing p c sc o f gen
         in renderAnnotatedCladogram lins idxTree
       else
         let root = ladderizeTree rawRoot
         in T.unlines (renderRootPlain root)
  where
    renderRootPlain (PhyloLeaf lbl mb) =
      [lbl <> formatAsciiBranch mb]
    renderRootPlain (PhyloNode l r mb) =
      let rootHeader = "+" <> formatAsciiBranch mb
      in rootHeader
         : renderSubtreePlain "" False l
        ++ renderSubtreePlain "" True  r

    renderSubtreePlain prefix isLast (PhyloLeaf lbl mb) =
      let connector = if isLast then "└── " else "├── "
      in [prefix <> connector <> lbl <> formatAsciiBranch mb]
    renderSubtreePlain prefix isLast (PhyloNode l r mb) =
      let connector   = if isLast then "└── " else "├── "
          childPrefix = prefix <> (if isLast then "    " else "│   ")
          nodeLabel   = "+" <> formatAsciiBranch mb
      in (prefix <> connector <> nodeLabel)
         : renderSubtreePlain childPrefix False l
        ++ renderSubtreePlain childPrefix True  r

--------------------------------------------------------------------------------
-- Operadores Estructurales para Búsqueda Topológica (Wagner, SPR, TBR)
--------------------------------------------------------------------------------

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
-- por cualquiera de sus aristas (operador requerido por TBR).
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

-- | Enumera todos los $2N - 3$ re-enraizados posibles de un árbol bifurcado preservando
-- las longitudes aditivas de las ramas cuando están presentes.
rerootWithBranchLengths :: PhyloTree a -> [PhyloTree a]
rerootWithBranchLengths tree =
  case tree of
    PhyloLeaf _ _ -> [tree]
    PhyloNode l r _ ->
      let combRoot = combineLengths (treeBranchLength l) (treeBranchLength r)
          lWithRoot = setBranchLength combRoot l
          rWithRoot = setBranchLength combRoot r
      in tree
         : descend l rWithRoot
        ++ descend r lWithRoot
  where
    combineLengths (Just a) (Just b) = Just (a + b)
    combineLengths (Just a) Nothing  = Just a
    combineLengths Nothing  (Just b) = Just b
    combineLengths Nothing  Nothing  = Nothing

    halfLength Nothing  = Nothing
    halfLength (Just x) = Just (x / 2.0)

    descend (PhyloLeaf _ _) _ = []
    descend (PhyloNode a b _) ctx =
      let mbA     = treeBranchLength a
          mbB     = treeBranchLength b
          halfA   = halfLength mbA
          halfB   = halfLength mbB
          rootAtA = PhyloNode (setBranchLength halfA a) (PhyloNode b ctx halfA) Nothing
          rootAtB = PhyloNode (setBranchLength halfB b) (PhyloNode a ctx halfB) Nothing
          ctxForA = PhyloNode b ctx mbA
          ctxForB = PhyloNode a ctx mbB
      in rootAtA
         : rootAtB
         : descend a ctxForA
        ++ descend b ctxForB

-- | Genera la vecindad SPR (*Subtree Pruning and Regrafting*) completa de un árbol,
-- deduplicando topologías idénticas mediante 'Set.fromList'.
sprNeighbors :: Ord a => PhyloTree a -> [PhyloTree a]
sprNeighbors tree =
  let clean = clearBranchLengths tree
  in Set.toList . Set.fromList $
       [ candidate
       | (pruned, residual) <- pruneAllSubtrees clean
       , candidate <- regraftSubtreeEverywhere pruned residual
       , candidate /= clean
       ]

-- | Genera la vecindad TBR (*Tree Bisection and Reconnection*) completa de un árbol,
-- deduplicando topologías idénticas mediante 'Set.fromList'.
tbrNeighbors :: Ord a => PhyloTree a -> [PhyloTree a]
tbrNeighbors tree =
  let clean = clearBranchLengths tree
  in Set.toList . Set.fromList $
       [ candidate
       | (pruned, residual) <- pruneAllSubtrees clean
       , rerooted <- rerootSubtreeAll pruned
       , candidate <- regraftSubtreeEverywhere rerooted residual
       , candidate /= clean
       ]
