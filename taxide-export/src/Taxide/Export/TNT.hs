{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Export.TNT
Description : Compilador a TNT (Goloboff, Farris & Nixon, 2008)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Genera un archivo @.tnt@ con bloque @xread@ (dimensiones exactas de caracteres
y taxones), polimorfismos entre corchetes @[12]@, nombres de caracteres y estados
(@cnames@) y el bloque @ccode@ (@+@ aditivo/ordenado, @-@ no aditivo/desordenado).
TNT no distingue polimorfismo de incertidumbre: ambos se emiten como @[..]@.
-}

module Taxide.Export.TNT
  ( TntOptions (..)
  , defaultTntOptions
  , renderTnt
  ) where

import Data.List (transpose)
import Data.Text (Text)
import qualified Data.Text as T

import Taxide.Core.Types (DatasetMetadata (..))
import Taxide.Engine.Discretize (DiscretizeMethod (..), DiscretizeTarget (..))
import Taxide.Engine.Variants (ResolvedTaxon (..))
import Taxide.Export.Common

-- | Opciones del compilador TNT.
data TntOptions = TntOptions
  { tntMethod :: !DiscretizeMethod
  , tntTarget :: !DiscretizeTarget
  } deriving (Eq, Show)

-- | Raíz cuadrada y valor central.
defaultTntOptions :: TntOptions
defaultTntOptions = TntOptions SquareRootRule UseCenterVal

-- | Compila la matriz resuelta a un documento TNT.
renderTnt :: TntOptions -> ExportContext -> Either ExportError Text
renderTnt opts ctx
  | null cols = Left (ExportError "TNT: no hay caracteres exportables (todos son TE o carecen de valores).")
  | null taxa = Left (ExportError "TNT: no hay taxones.")
  | maxStates > length stateSymbolsTnt =
      Left (ExportError ("TNT: un carácter tiene " <> tshow maxStates
                         <> " estados; el máximo representable es " <> tshow (length stateSymbolsTnt) <> "."))
  | otherwise = Right (T.unlines (concat [prelude, xreadBlock, cnamesBlock, ccodeBlock, epilogue]))
  where
    cols = buildCladColumns (tntMethod opts) (tntTarget opts) ctx
    taxa = ecTaxa ctx
    names = map (sanitizeToken . rtName) taxa
    maxStates = maximum (map (length . ccStateLabels) cols)

    prelude = [ "nstates 32;" ]

    xreadBlock =
      [ "xread"
      , "'" <> T.replace "'" "" (title) <> "'"
      , tshow (length cols) <> " " <> tshow (length taxa)
      ] ++
      [ nm <> " " <> T.concat (map renderCell cells)
      | (nm, cells) <- zip names (transpose (map ccCells cols))
      ] ++
      [ ";" ]

    title = "TAXIDE: " <> T.map (\c -> if c == '\n' then ' ' else c) (datasetTitle (ecMetadata ctx))

    renderCell cell = case cell of
      CellMissing -> "?"
      CellGap     -> "-"
      CellStates kind ss ->
        let body = T.pack [stateSymbolsTnt !! s | s <- ss]
        in case kind of
             VkSingle -> body
             _        -> "[" <> body <> "]"

    cnamesBlock =
      [ "cnames" ] ++
      [ "{" <> tshow i <> " " <> sanitizeToken (ccLabel c) <> " "
          <> T.unwords (map sanitizeToken (ccStateLabels c)) <> ";"
      | (i, c) <- zip [0 :: Int ..] cols
      ] ++
      [ ";" ]

    ccodeBlock =
      let idxs = zip [0 :: Int ..] cols
          additive = [i | (i, c) <- idxs, ccOrdered c]
          nonAdd   = [i | (i, c) <- idxs, not (ccOrdered c)]
      in [ "ccode " <> T.unwords ([ "+ " <> T.unwords (map tshow additive) | not (null additive) ]
                                  ++ [ "- " <> T.unwords (map tshow nonAdd) | not (null nonAdd) ])
           <> ";" ]

    epilogue = [ "proc /;" ]

    tshow :: Show a => a -> Text
    tshow = T.pack . show
