{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Export.Common
Description : Contexto de exportación, columnas cladísticas y utilidades de formato
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Núcleo compartido por todos los compiladores de la Fase 3. Reúne los insumos
inmutables de la exportación ('ExportContext'), proyecta la matriz resuelta a
columnas cladísticas (discretizando los caracteres @IN@/@RN@ a @OM@) y ofrece
las utilidades puras de escape y formato numérico. No realiza ninguna E/S.
-}

module Taxide.Export.Common
  ( -- * Errores
    ExportError (..)
  , renderExportError

    -- * Contexto de exportación
  , ExportContext (..)
  , mkExportContext
  , ctxCharDef
  , ctxCharType
  , ctxFeature
  , ctxStateLabel
  , ctxUnits

    -- * Semántica de expresiones de estados
  , ValueKind (..)
  , exprKind
  , exprStates
  , exprComments

    -- * Columnas cladísticas (NEXUS / TNT)
  , Cell (..)
  , CladColumn (..)
  , buildCladColumns
  , stateSymbolsNexus
  , stateSymbolsTnt

    -- * Utilidades de formato
  , fmtNum
  , fmtStatRange
  , escapeXml
  , escapeHtml
  , capitalizeFirst
  , compressRanges
  , sanitizeToken
  , quoteNexus
  , tsvField
  , slugify
  ) where

import Data.Char (isAlphaNum, isSpace, toLower, toUpper)
import Data.Foldable (toList)
#if __GLASGOW_HASKELL__ < 910
import Data.List (foldl')
#endif
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)

import Taxide.Core.Character (CharacterDef (..))
import Taxide.Core.Invariants (collectActiveStates)
import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Core.Types
  ( CharacterType (..)
  , DatasetMetadata
  , PseudoValue (..)
  , RawAttribute (..)
  , RawStateExpr (..)
  , StatRange (..)
  , StateId
  , StateToken (..)
  )
import Taxide.Engine.Discretize
  ( DiscretizeMethod
  , DiscretizeTarget
  , DiscretizedCharacter (..)
  , discretizeTaxaValues
  )
import Taxide.Engine.Matrix (ColumnarMatrix)
import Taxide.Engine.Variants (AttributedState (..), ResolvedTaxon (..))

--------------------------------------------------------------------------------
-- Errores
--------------------------------------------------------------------------------

-- | Error formal de exportación (formato incapaz de representar la matriz).
newtype ExportError = ExportError Text
  deriving (Eq, Show)

-- | Mensaje legible del error.
renderExportError :: ExportError -> Text
renderExportError (ExportError msg) = msg

--------------------------------------------------------------------------------
-- Contexto de exportación
--------------------------------------------------------------------------------

-- | Insumos inmutables de cualquier compilador de salida.
data ExportContext = ExportContext
  { ecSpecs    :: !SpecsDef
  , ecChars    :: ![CharacterDef]
  , ecCharMap  :: !(Map Int CharacterDef)
  , ecTaxa     :: ![ResolvedTaxon]
  , ecMatrix   :: !ColumnarMatrix
  , ecMetadata :: !DatasetMetadata
  } deriving (Eq, Show)

-- | Construye el contexto a partir de las salidas del pipeline semántico.
mkExportContext
  :: DatasetMetadata
  -> SpecsDef
  -> [CharacterDef]
  -> [ResolvedTaxon]
  -> ColumnarMatrix
  -> ExportContext
mkExportContext meta specs chars taxa mat = ExportContext
  { ecSpecs    = specs
  , ecChars    = chars
  , ecCharMap  = Map.fromList [(charNumber c, c) | c <- chars]
  , ecTaxa     = taxa
  , ecMatrix   = mat
  , ecMetadata = meta
  }

-- | Definición CHARS del carácter, si existe.
ctxCharDef :: ExportContext -> Int -> Maybe CharacterDef
ctxCharDef ctx cid = Map.lookup cid (ecCharMap ctx)

-- | Tipo efectivo del carácter (SPECS, luego CHARS, por defecto @UM@).
ctxCharType :: ExportContext -> Int -> CharacterType
ctxCharType ctx cid =
  case Map.lookup cid (specCharTypes (ecSpecs ctx)) of
    Just t  -> t
    Nothing -> maybe UM charType (ctxCharDef ctx cid)

-- | Nombre de la característica (con respaldo @character N@).
ctxFeature :: ExportContext -> Int -> Text
ctxFeature ctx cid =
  case ctxCharDef ctx cid of
    Just c | not (T.null (T.strip (charFeature c))) -> T.strip (charFeature c)
    _ -> "character " <> T.pack (show cid)

-- | Etiqueta textual de un estado (con respaldo @state N@).
ctxStateLabel :: ExportContext -> Int -> StateId -> Text
ctxStateLabel ctx cid s =
  case ctxCharDef ctx cid >>= lookup s . charStates of
    Just lbl | not (T.null (T.strip lbl)) -> T.strip lbl
    _ -> "state " <> T.pack (show s)

-- | Unidades dimensionales declaradas en CHARS.
ctxUnits :: ExportContext -> Int -> Maybe Text
ctxUnits ctx cid = ctxCharDef ctx cid >>= charUnits

--------------------------------------------------------------------------------
-- Semántica de expresiones de estados
--------------------------------------------------------------------------------

-- | Naturaleza lógica de una celda cualitativa.
data ValueKind
  = VkSingle       -- ^ Un único estado
  | VkPolymorphic  -- ^ Polimorfismo simultáneo (@&@): el taxón posee todos los estados
  | VkUncertain    -- ^ Incertidumbre o alternativa (@/@, rangos): uno de varios estados
  deriving (Eq, Show)

-- | Clasifica la expresión: cualquier @&@ implica polimorfismo; varios estados
-- sin @&@ implican incertidumbre.
exprKind :: RawStateExpr -> ValueKind
exprKind expr
  | Set.size (collectActiveStates expr) <= 1 = VkSingle
  | hasAnd expr                              = VkPolymorphic
  | otherwise                                = VkUncertain
  where
    hasAnd e = case e of
      StateAnd _ _ -> True
      StateOr a b  -> hasAnd a || hasAnd b
      _            -> False

-- | Estados activos en orden ascendente.
exprStates :: RawStateExpr -> [StateId]
exprStates = toList . collectActiveStates

-- | Comentarios contextuales de los tokens de estado.
exprComments :: RawStateExpr -> [Text]
exprComments expr = case expr of
  SingleState t   -> maybe [] pure (stComment t)
  RangeTo a b     -> mapMaybe stComment [a, b]
  StateOr a b     -> exprComments a ++ exprComments b
  StateAnd a b    -> exprComments a ++ exprComments b
  PseudoState _ c -> maybe [] pure c

--------------------------------------------------------------------------------
-- Columnas cladísticas
--------------------------------------------------------------------------------

-- | Celda cladística independiente del formato (estados en base 0).
data Cell
  = CellMissing                 -- ^ Desconocido (@?@)
  | CellGap                     -- ^ Inaplicable (@-@)
  | CellStates !ValueKind ![Int] -- ^ Uno o varios estados (base 0, ascendentes)
  deriving (Eq, Show)

-- | Carácter proyectado a una columna de la matriz cladística.
data CladColumn = CladColumn
  { ccChar        :: !Int      -- ^ Número de carácter original
  , ccLabel       :: !Text     -- ^ Etiqueta del carácter
  , ccOrdered     :: !Bool     -- ^ Ordenado (@OM@/@EOM@/cuantitativo discretizado)
  , ccDerived     :: !Bool     -- ^ Proviene de discretizar @IN@/@RN@
  , ccStateLabels :: ![Text]   -- ^ Etiquetas de los estados 0..k-1
  , ccCells       :: ![Cell]   -- ^ Una celda por taxón, en orden de matriz
  } deriving (Eq, Show)

-- | Alfabeto de símbolos NEXUS (62 estados).
stateSymbolsNexus :: String
stateSymbolsNexus = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']

-- | Alfabeto de símbolos TNT (32 estados: @0-9@, @A-V@).
stateSymbolsTnt :: String
stateSymbolsTnt = ['0' .. '9'] ++ ['A' .. 'V']

-- | Proyecta todos los caracteres exportables a columnas cladísticas. Los
-- caracteres @TE@ se omiten; los @IN@/@RN@ se discretizan a @OM@ con el método
-- indicado (columnas sin ningún valor se descartan).
buildCladColumns :: DiscretizeMethod -> DiscretizeTarget -> ExportContext -> [CladColumn]
buildCladColumns method target ctx = concatMap build [1 .. specNumCharacters (ecSpecs ctx)]
  where
    taxa = ecTaxa ctx

    build cid = case ctxCharType ctx cid of
      TE  -> []
      IN  -> quantitative cid
      RN  -> quantitative cid
      t   -> [discrete cid (t `elem` [OM, EOM])]

    attrOf cid t = Map.lookup cid (rtAttributes t)

    observedStates cid = Set.unions
      [ collectActiveStates expr
      | t <- taxa
      , Just (AttributedState (AttrStates _ expr) _) <- [attrOf cid t]
      ]

    numStates cid =
      let declared = Map.findWithDefault 0 cid (specNumbersOfStates (ecSpecs ctx))
          fromChars = maybe 0 (\c -> max (length (charStates c))
                                         (maximum (0 : map (fromIntegral . fst) (charStates c))))
                            (ctxCharDef ctx cid)
          observed = if Set.null (observedStates cid) then 0 else fromIntegral (Set.findMax (observedStates cid))
      in maximum [declared, fromChars, observed, 1]

    discrete cid ordered =
      let n = numStates cid
          allStates = [0 .. n - 1]
          cellFor t = case attrOf cid t of
            Nothing -> CellMissing
            Just (AttributedState (AttrStates _ expr) _) -> classify allStates expr
            Just _ -> CellMissing
      in CladColumn
           { ccChar        = cid
           , ccLabel       = ctxFeature ctx cid
           , ccOrdered     = ordered
           , ccDerived     = False
           , ccStateLabels = [ctxStateLabel ctx cid (fromIntegral s) | s <- [1 .. n]]
           , ccCells       = map cellFor taxa
           }

    classify allStates expr = case expr of
      PseudoState Unknown _      -> CellMissing
      PseudoState Inapplicable _ -> CellGap
      PseudoState Variable _     -> CellStates VkPolymorphic allStates
      _ -> case exprStates expr of
             []     -> CellMissing
             states -> CellStates (exprKind expr) [fromIntegral s - 1 | s <- states]

    quantitative cid =
      let dc = discretizeTaxaValues cid method target taxa
          k = dcNumClasses dc
          units = maybe "" (" " <>) (ctxUnits ctx cid)
          cellFor t = case Map.lookup (rtIndex t) (dcAssignments dc) of
            Just s  -> CellStates VkSingle [fromIntegral s - 1]
            Nothing -> case attrOf cid t of
              Just (AttributedState (AttrStates _ (PseudoState Inapplicable _)) _) -> CellGap
              _ -> CellMissing
          bounds = dcBoundaries dc
          labelFor i
            | k == 1 = "= " <> fmtNum (head' bounds) <> units
            | otherwise =
                let lo = bounds !! i
                    hi = bounds !! (i + 1)
                    close = if i == k - 1 then "]" else ")"
                in "[" <> fmtNum lo <> ", " <> fmtNum hi <> close <> units
          head' xs = fromMaybe 0 (safeHead xs)
      in if k == 0 || length bounds < 2
           then []
           else [ CladColumn
                    { ccChar        = cid
                    , ccLabel       = ctxFeature ctx cid
                    , ccOrdered     = True
                    , ccDerived     = True
                    , ccStateLabels = map labelFor [0 .. k - 1]
                    , ccCells       = map cellFor taxa
                    } ]

    safeHead xs = case xs of
      (x : _) -> Just x
      []      -> Nothing

--------------------------------------------------------------------------------
-- Utilidades de formato
--------------------------------------------------------------------------------

-- | Número compacto: enteros sin parte decimal y decimales sin ceros finales.
fmtNum :: Double -> Text
fmtNum d
  | isNaN d || isInfinite d = T.pack (show d)
  | d == fromIntegral (round d :: Integer) = T.pack (show (round d :: Integer))
  | otherwise =
      let s = showFFloat (Just 3) d ""
          trimmed = reverse (dropWhile (== '0') (reverse s))
      in T.pack (if last trimmed == '.' then init trimmed else trimmed)

-- | Rango normal @n1-n2@ (o @n@), con valor central y extremos absolutos
-- opcionales. Devuelve el valor principal y las observaciones asociadas.
fmtStatRange :: StatRange Double -> (Text, [Text])
fmtStatRange sr =
  let lo = srNormalMin sr
      hi = srNormalMax sr
      main = if lo == hi then fmtNum lo else fmtNum lo <> "-" <> fmtNum hi
      centre = maybe [] (\c -> ["centre " <> fmtNum c]) (srCenterVal sr)
      extremes = case (srAbsoluteMin sr, srAbsoluteMax sr) of
        (Nothing, Nothing) -> []
        (Just a, Just b)  -> [ "extremes " <> fmtNum a <> "-" <> fmtNum b ]
        (Just a, Nothing) -> [ "minimum " <> fmtNum a ]
        (Nothing, Just b) -> [ "maximum " <> fmtNum b ]
  in (main, centre ++ extremes)

-- | Escape de los cinco caracteres reservados de XML.
escapeXml :: Text -> Text
escapeXml = T.concatMap esc
  where
    esc c = case c of
      '&'  -> "&amp;"
      '<'  -> "&lt;"
      '>'  -> "&gt;"
      '"'  -> "&quot;"
      '\'' -> "&apos;"
      _    -> T.singleton c

-- | Escape de HTML5 (mismo conjunto que XML).
escapeHtml :: Text -> Text
escapeHtml = escapeXml

-- | Primera letra en mayúscula.
capitalizeFirst :: Text -> Text
capitalizeFirst t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing        -> t

-- | Comprime enteros ascendentes en rangos @a-b@ separados por espacio.
compressRanges :: [Int] -> Text
compressRanges xs = T.unwords (map render (groupRuns xs))
  where
    render (a, b) | a == b    = T.pack (show a)
                  | otherwise = T.pack (show a) <> "-" <> T.pack (show b)
    groupRuns [] = []
    groupRuns (y : ys) = reverse (foldl' step [(y, y)] ys)
    step acc@((a, b) : rest) v
      | v == b + 1 = (a, v) : rest
      | otherwise  = (v, v) : acc
    step [] v = [(v, v)]

-- | Token sin espacios ni delimitadores (nombres de taxón en TNT).
sanitizeToken :: Text -> Text
sanitizeToken = collapse . T.map fix . T.strip
  where
    -- Colapsa guiones bajos repetidos y recorta los de los extremos.
    collapse = T.dropAround (== '_') . T.intercalate "_" . filter (not . T.null) . T.splitOn "_"
    fix c
      | isSpace c || c `elem` (";,()[]{}'\"=" :: String) = '_'
      | otherwise = c

-- | Etiqueta NEXUS entre comillas simples (apóstrofes duplicados).
quoteNexus :: Text -> Text
quoteNexus t = "'" <> T.replace "'" "''" (T.map (\c -> if c == '\n' || c == '\r' || c == '\t' then ' ' else c) t) <> "'"

-- | Campo TSV: tabuladores y saltos de línea se normalizan a espacio.
tsvField :: Text -> Text
tsvField = T.strip . T.map (\c -> if c == '\t' || c == '\n' || c == '\r' then ' ' else c)

-- | Identificador estable derivado de un nombre (minúsculas, guiones).
slugify :: Text -> Text
slugify t =
  let mapped = T.map (\c -> if isAlphaNum c then toLower c else '-') t
      collapsed = T.intercalate "-" (filter (not . T.null) (T.splitOn "-" mapped))
  in if T.null collapsed then "taxon" else collapsed
