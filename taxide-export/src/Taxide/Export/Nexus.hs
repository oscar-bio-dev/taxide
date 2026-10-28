{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Export.Nexus
Description : Compilador a NEXUS (Maddison, Swofford & Maddison, 1997)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Genera un archivo @.nex@ con los bloques @TAXA@, @CHARACTERS@ y @ASSUMPTIONS@.
La @MATRIX@ distingue el polimorfismo simultáneo @(12)@ (operador @&@ de DELTA)
de la incertidumbre @{12}@ (operador @/@ y rangos). Los caracteres @IN@/@RN@ se
discretizan a multiestado ordenado.
-}

module Taxide.Export.Nexus
  ( NexusOptions (..)
  , defaultNexusOptions
  , renderNexus
  ) where

import Data.List (transpose)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Taxide.Engine.Discretize (DiscretizeMethod (..), DiscretizeTarget (..))
import Taxide.Engine.Variants (ResolvedTaxon (..))
import Taxide.Export.Common

-- | Opciones del compilador NEXUS.
data NexusOptions = NexusOptions
  { nexMethod :: !DiscretizeMethod  -- ^ Regla de discretización de @IN@/@RN@
  , nexTarget :: !DiscretizeTarget  -- ^ Valor representativo del rango
  } deriving (Eq, Show)

-- | Raíz cuadrada y valor central.
defaultNexusOptions :: NexusOptions
defaultNexusOptions = NexusOptions SquareRootRule UseCenterVal

-- | Compila la matriz resuelta a un documento NEXUS.
renderNexus :: NexusOptions -> ExportContext -> Either ExportError Text
renderNexus opts ctx
  | null cols = Left (ExportError "NEXUS: no hay caracteres exportables (todos son TE o carecen de valores).")
  | maxStates > length stateSymbolsNexus =
      Left (ExportError ("NEXUS: un carácter tiene " <> tshow maxStates
                         <> " estados; el máximo representable es " <> tshow (length stateSymbolsNexus) <> "."))
  | otherwise = Right (T.unlines (concat [header, taxaBlock, charsBlock, assumptionsBlock]))
  where
    cols = buildCladColumns (nexMethod opts) (nexTarget opts) ctx
    taxa = ecTaxa ctx
    names = map rtName taxa
    maxStates = maximum (map (length . ccStateLabels) cols)
    symbols = take (max 2 maxStates) stateSymbolsNexus

    skipped = [ c | c <- Map.keys (ecCharMap ctx), c `notElem` map ccChar cols ]

    header =
      [ "#NEXUS"
      , "[Generado por TAXIDE taxide-compile. Matriz resuelta: "
          <> tshow (length taxa) <> " taxones, " <> tshow (length cols) <> " caracteres."
      , " Polimorfismo simultaneo: (12); incertidumbre: {12}; ? desconocido; - inaplicable."
      , " Caracteres omitidos (TE o sin valores): " <> (if null skipped then "ninguno" else T.unwords (map tshow skipped)) <> ".]"
      , ""
      ]

    taxaBlock =
      [ "BEGIN TAXA;"
      , "  DIMENSIONS NTAX=" <> tshow (length taxa) <> ";"
      , "  TAXLABELS"
      ] ++ map (("    " <>) . quoteNexus) names ++
      [ "  ;"
      , "END;"
      , ""
      ]

    charsBlock =
      [ "BEGIN CHARACTERS;"
      , "  DIMENSIONS NCHAR=" <> tshow (length cols) <> ";"
      , "  FORMAT DATATYPE=STANDARD SYMBOLS=\"" <> T.pack symbols <> "\" MISSING=? GAP=-;"
      , "  CHARLABELS"
      ] ++ map (("    " <>) . quoteNexus . ccLabel) cols ++
      [ "  ;"
      , "  STATELABELS"
      ] ++ stateLabelLines ++
      [ "  ;"
      , "  MATRIX"
      ] ++ matrixLines ++
      [ "  ;"
      , "END;"
      , ""
      ]

    stateLabelLines =
      let entries = [ "    " <> tshow i <> " " <> T.unwords (map quoteNexus (ccStateLabels c))
                    | (i, c) <- zip [1 :: Int ..] cols ]
      in case entries of
           [] -> []
           _  -> map (<> ",") (init entries) ++ [last entries]

    padWidth = maximum (map (T.length . quoteNexus) names ++ [0]) + 2
    rows = if null taxa then [] else transpose (map ccCells cols)
    matrixLines =
      [ "    " <> T.justifyLeft padWidth ' ' (quoteNexus nm)
          <> T.concat (map renderCell cells)
      | (nm, cells) <- zip names rows
      ]

    renderCell cell = case cell of
      CellMissing -> "?"
      CellGap     -> "-"
      CellStates kind ss ->
        let body = T.pack [symbols !! s | s <- ss]
        in case kind of
             VkSingle      -> body
             VkPolymorphic -> "(" <> body <> ")"
             VkUncertain   -> "{" <> body <> "}"

    assumptionsBlock =
      let idxs = zip [1 :: Int ..] cols
          unord = [i | (i, c) <- idxs, not (ccOrdered c)]
          ord'  = [i | (i, c) <- idxs, ccOrdered c]
          parts = [ "unord: " <> compressRanges unord | not (null unord) ]
               ++ [ "ord: " <> compressRanges ord' | not (null ord') ]
      in [ "BEGIN ASSUMPTIONS;"
         , "  TYPESET taxide_types = " <> T.intercalate ", " parts <> ";"
         , "END;"
         ]

    tshow :: Show a => a -> Text
    tshow = T.pack . show
