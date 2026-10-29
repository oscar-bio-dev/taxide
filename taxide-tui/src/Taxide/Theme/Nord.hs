{-# LANGUAGE StrictData #-}

{- |
Module      : Taxide.Theme.Nord
Description : Tema Arctic Nord (Polar Night, Snow Storm, Frost & Aurora 24-bit TrueColor)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Especificación cromática Nord (@:theme nord@) basada en tonos árticos azulados de
baja fatiga visual.
-}

module Taxide.Theme.Nord
  ( -- * Colores RGB de 24 bits (Nord)
    nordBg0
  , nordBgHard
  , nordBg1
  , nordBg2
  , nordFg0
  , nordGreen
  , nordAqua
  , nordGray
  , nordOrange
  , nordRed
  , nordYellow

    -- * Paleta y AttrMap
  , nordPalette
  , nordAttrMap
  ) where

import Brick (AttrMap)
import qualified Graphics.Vty as Vty

import Taxide.Theme.Types (ThemePalette (..), buildThemeAttrMap)

-- | Fondo Polar Night @nord0@ @#2e3440@ $(46, 52, 64)$.
nordBg0 :: Vty.Color
nordBg0 = Vty.rgbColor (46 :: Int) 52 64

-- | Fondo profundo Polar Night para barras y cabeceras @#242933@ $(36, 41, 51)$.
nordBgHard :: Vty.Color
nordBgHard = Vty.rgbColor (36 :: Int) 41 51

-- | Superficie elevada @nord1@ @#3b4252@ $(59, 66, 82)$.
nordBg1 :: Vty.Color
nordBg1 = Vty.rgbColor (59 :: Int) 66 82

-- | Cursor activo @nord3@ @#4c566a@ $(76, 86, 106)$.
nordBg2 :: Vty.Color
nordBg2 = Vty.rgbColor (76 :: Int) 86 106

-- | Texto Snow Storm @nord6@ @#eceff4@ $(236, 239, 244)$.
nordFg0 :: Vty.Color
nordFg0 = Vty.rgbColor (236 :: Int) 239 244

-- | Estado aplicable válido @nord14@ (Aurora Green) @#a3be8c@ $(163, 190, 140)$.
nordGreen :: Vty.Color
nordGreen = Vty.rgbColor (163 :: Int) 190 140

-- | Valor implícito o heredado @nord8@ (Frost Cyan) @#88c0d0@ $(136, 192, 208)$.
nordAqua :: Vty.Color
nordAqua = Vty.rgbColor (136 :: Int) 192 208

-- | Pseudo-valor inaplicable @-@ (Polar Slate) @#7b88a1@ $(123, 136, 161)$.
nordGray :: Vty.Color
nordGray = Vty.rgbColor (123 :: Int) 136 161

-- | Incertidumbre @U@/@V@ o advertencia @nord12@ (Aurora Orange) @#d08770@ $(208, 135, 112)$.
nordOrange :: Vty.Color
nordOrange = Vty.rgbColor (208 :: Int) 135 112

-- | Violación biológica del linter @nord11@ (Aurora Red) @#bf616a@ $(191, 97, 106)$.
nordRed :: Vty.Color
nordRed = Vty.rgbColor (191 :: Int) 97 106

-- | Acento @nord13@ (Aurora Yellow) @#ebcb8b@ $(235, 203, 139)$.
nordYellow :: Vty.Color
nordYellow = Vty.rgbColor (235 :: Int) 203 139

-- | Estructura de paleta completa para Nord.
nordPalette :: ThemePalette
nordPalette = ThemePalette
  { tpBg0    = nordBg0
  , tpBgHard = nordBgHard
  , tpBg1    = nordBg1
  , tpBg2    = nordBg2
  , tpFg0    = nordFg0
  , tpGreen  = nordGreen
  , tpAqua   = nordAqua
  , tpGray   = nordGray
  , tpOrange = nordOrange
  , tpRed    = nordRed
  , tpYellow = nordYellow
  }

-- | Mapa de atributos de Brick para Nord.
nordAttrMap :: AttrMap
nordAttrMap = buildThemeAttrMap nordPalette

