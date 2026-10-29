{-# LANGUAGE StrictData #-}

{- |
Module      : Taxide.Theme.Dracula
Description : Tema Dracula Dark (24-bit TrueColor)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Especificación cromática Dracula (@:theme dracula@) con alto contraste sobre
fondo púrpura oscuro.
-}

module Taxide.Theme.Dracula
  ( -- * Colores RGB de 24 bits (Dracula)
    draculaBg0
  , draculaBgHard
  , draculaBg1
  , draculaBg2
  , draculaFg0
  , draculaGreen
  , draculaAqua
  , draculaGray
  , draculaOrange
  , draculaRed
  , draculaYellow

    -- * Paleta y AttrMap
  , draculaPalette
  , draculaAttrMap
  ) where

import Brick (AttrMap)
import qualified Graphics.Vty as Vty

import Taxide.Theme.Types (ThemePalette (..), buildThemeAttrMap)

-- | Fondo base Dracula @#282a36@ $(40, 42, 54)$.
draculaBg0 :: Vty.Color
draculaBg0 = Vty.rgbColor (40 :: Int) 42 54

-- | Fondo profundo para barras y cabeceras @#1e1f29@ $(30, 31, 41)$.
draculaBgHard :: Vty.Color
draculaBgHard = Vty.rgbColor (30 :: Int) 31 41

-- | Superficie elevada para modales y fila activa @#343746@ $(52, 55, 70)$.
draculaBg1 :: Vty.Color
draculaBg1 = Vty.rgbColor (52 :: Int) 55 70

-- | Selección y cursor activo @#44475a@ $(68, 71, 90)$.
draculaBg2 :: Vty.Color
draculaBg2 = Vty.rgbColor (68 :: Int) 71 90

-- | Texto primario @#f8f8f2@ $(248, 248, 242)$.
draculaFg0 :: Vty.Color
draculaFg0 = Vty.rgbColor (248 :: Int) 248 242

-- | Estado aplicable válido @#50fa7b@ $(80, 250, 123)$.
draculaGreen :: Vty.Color
draculaGreen = Vty.rgbColor (80 :: Int) 250 123

-- | Valor implícito o heredado @#8be9fd@ $(139, 233, 253)$.
draculaAqua :: Vty.Color
draculaAqua = Vty.rgbColor (139 :: Int) 233 253

-- | Pseudo-valor inaplicable @-@ @#6272a4@ $(98, 114, 164)$.
draculaGray :: Vty.Color
draculaGray = Vty.rgbColor (98 :: Int) 114 164

-- | Incertidumbre @U@/@V@ o advertencia @#ffb86c@ $(255, 184, 108)$.
draculaOrange :: Vty.Color
draculaOrange = Vty.rgbColor (255 :: Int) 184 108

-- | Violación biológica del linter @#ff5555@ $(255, 85, 85)$.
draculaRed :: Vty.Color
draculaRed = Vty.rgbColor (255 :: Int) 85 85

-- | Acento amarillo @#f1fa8c@ $(241, 250, 140)$.
draculaYellow :: Vty.Color
draculaYellow = Vty.rgbColor (241 :: Int) 250 140

-- | Estructura de paleta completa para Dracula.
draculaPalette :: ThemePalette
draculaPalette = ThemePalette
  { tpBg0    = draculaBg0
  , tpBgHard = draculaBgHard
  , tpBg1    = draculaBg1
  , tpBg2    = draculaBg2
  , tpFg0    = draculaFg0
  , tpGreen  = draculaGreen
  , tpAqua   = draculaAqua
  , tpGray   = draculaGray
  , tpOrange = draculaOrange
  , tpRed    = draculaRed
  , tpYellow = draculaYellow
  }

-- | Mapa de atributos de Brick para Dracula.
draculaAttrMap :: AttrMap
draculaAttrMap = buildThemeAttrMap draculaPalette

