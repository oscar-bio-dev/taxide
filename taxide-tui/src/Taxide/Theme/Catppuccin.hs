{-# LANGUAGE StrictData #-}

{- |
Module      : Taxide.Theme.Catppuccin
Description : Tema Catppuccin Mocha (24-bit TrueColor)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Especificación cromática Catppuccin Mocha (@:theme catppuccin@) con tonos pastel
cálidos sobre base nocturna.
-}

module Taxide.Theme.Catppuccin
  ( -- * Colores RGB de 24 bits (Catppuccin Mocha)
    catppuccinBg0
  , catppuccinBgHard
  , catppuccinBg1
  , catppuccinBg2
  , catppuccinFg0
  , catppuccinGreen
  , catppuccinAqua
  , catppuccinGray
  , catppuccinOrange
  , catppuccinRed
  , catppuccinYellow

    -- * Paleta y AttrMap
  , catppuccinPalette
  , catppuccinAttrMap
  ) where

import Brick (AttrMap)
import qualified Graphics.Vty as Vty

import Taxide.Theme.Types (ThemePalette (..), buildThemeAttrMap)

-- | Base Catppuccin Mocha @#1e1e2e@ $(30, 30, 46)$.
catppuccinBg0 :: Vty.Color
catppuccinBg0 = Vty.rgbColor (30 :: Int) 30 46

-- | Mantle/Crust profundo para barras y cabeceras @#181825@ $(24, 24, 37)$.
catppuccinBgHard :: Vty.Color
catppuccinBgHard = Vty.rgbColor (24 :: Int) 24 37

-- | Surface0 para modales y fila activa @#313244@ $(49, 50, 68)$.
catppuccinBg1 :: Vty.Color
catppuccinBg1 = Vty.rgbColor (49 :: Int) 50 68

-- | Surface1 para cursor activo @#45475a@ $(69, 71, 90)$.
catppuccinBg2 :: Vty.Color
catppuccinBg2 = Vty.rgbColor (69 :: Int) 71 90

-- | Texto primario @#cdd6f4@ $(205, 214, 244)$.
catppuccinFg0 :: Vty.Color
catppuccinFg0 = Vty.rgbColor (205 :: Int) 214 244

-- | Estado aplicable válido (Green) @#a6e3a1@ $(166, 227, 161)$.
catppuccinGreen :: Vty.Color
catppuccinGreen = Vty.rgbColor (166 :: Int) 227 161

-- | Valor implícito o heredado (Teal) @#94e2d5@ $(148, 226, 213)$.
catppuccinAqua :: Vty.Color
catppuccinAqua = Vty.rgbColor (148 :: Int) 226 213

-- | Pseudo-valor inaplicable @-@ (Overlay1) @#7f849c@ $(127, 132, 156)$.
catppuccinGray :: Vty.Color
catppuccinGray = Vty.rgbColor (127 :: Int) 132 156

-- | Incertidumbre @U@/@V@ o advertencia (Peach) @#fab387@ $(250, 179, 135)$.
catppuccinOrange :: Vty.Color
catppuccinOrange = Vty.rgbColor (250 :: Int) 179 135

-- | Violación biológica del linter (Red) @#f38ba8@ $(243, 139, 168)$.
catppuccinRed :: Vty.Color
catppuccinRed = Vty.rgbColor (243 :: Int) 139 168

-- | Acento (Yellow) @#f9e2af@ $(249, 226, 175)$.
catppuccinYellow :: Vty.Color
catppuccinYellow = Vty.rgbColor (249 :: Int) 226 175

-- | Estructura de paleta completa para Catppuccin Mocha.
catppuccinPalette :: ThemePalette
catppuccinPalette = ThemePalette
  { tpBg0    = catppuccinBg0
  , tpBgHard = catppuccinBgHard
  , tpBg1    = catppuccinBg1
  , tpBg2    = catppuccinBg2
  , tpFg0    = catppuccinFg0
  , tpGreen  = catppuccinGreen
  , tpAqua   = catppuccinAqua
  , tpGray   = catppuccinGray
  , tpOrange = catppuccinOrange
  , tpRed    = catppuccinRed
  , tpYellow = catppuccinYellow
  }

-- | Mapa de atributos de Brick para Catppuccin Mocha.
catppuccinAttrMap :: AttrMap
catppuccinAttrMap = buildThemeAttrMap catppuccinPalette

