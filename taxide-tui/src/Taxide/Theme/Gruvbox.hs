{-# LANGUAGE StrictData #-}

{- |
Module      : Taxide.Theme.Gruvbox
Description : Tema por defecto Gruvbox Dark (24-bit TrueColor)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Especificación cromática Gruvbox Dark (@:theme gruvbox@), utilizada por defecto
al iniciar @taxide-tui@.
-}

module Taxide.Theme.Gruvbox
  ( -- * Colores RGB de 24 bits (Gruvbox Dark)
    gruvboxBg0
  , gruvboxBgHard
  , gruvboxBg1
  , gruvboxBg2
  , gruvboxFg0
  , gruvboxGreen
  , gruvboxAqua
  , gruvboxGray
  , gruvboxOrange
  , gruvboxRed
  , gruvboxYellow

    -- * Paleta y AttrMap
  , gruvboxPalette
  , gruvboxAttrMap
  ) where

import Brick (AttrMap)
import qualified Graphics.Vty as Vty

import Taxide.Theme.Types (ThemePalette (..), buildThemeAttrMap)

-- | Fondo base carbón @#282828@ $(40, 40, 40)$.
gruvboxBg0 :: Vty.Color
gruvboxBg0 = Vty.rgbColor (40 :: Int) 40 40

-- | Fondo de alto contraste @#1d2021@ $(29, 32, 33)$ para encabezados y barras.
gruvboxBgHard :: Vty.Color
gruvboxBgHard = Vty.rgbColor (29 :: Int) 32 33

-- | Fondo de resalte suave @#3c3836@ $(60, 56, 54)$.
gruvboxBg1 :: Vty.Color
gruvboxBg1 = Vty.rgbColor (60 :: Int) 56 54

-- | Fondo de cursor activo @#504945@ $(80, 73, 69)$.
gruvboxBg2 :: Vty.Color
gruvboxBg2 = Vty.rgbColor (80 :: Int) 73 69

-- | Texto primario crema @#ebdbb2@ $(235, 219, 178)$.
gruvboxFg0 :: Vty.Color
gruvboxFg0 = Vty.rgbColor (235 :: Int) 219 178

-- | Estado aplicable válido @#b8bb26@ $(184, 187, 38)$.
gruvboxGreen :: Vty.Color
gruvboxGreen = Vty.rgbColor (184 :: Int) 187 38

-- | Valor implícito o heredado de variante @#83a598@ $(131, 165, 152)$.
gruvboxAqua :: Vty.Color
gruvboxAqua = Vty.rgbColor (131 :: Int) 165 152

-- | Pseudo-valor inaplicable @-@ @#928374@ $(146, 131, 116)$.
gruvboxGray :: Vty.Color
gruvboxGray = Vty.rgbColor (146 :: Int) 131 116

-- | Incertidumbre total @U@/@V@ o advertencia del linter @#fe8019@ $(254, 128, 25)$.
gruvboxOrange :: Vty.Color
gruvboxOrange = Vty.rgbColor (254 :: Int) 128 25

-- | Violación biológica o matemática del linter @#fb4934@ $(251, 73, 52)$.
gruvboxRed :: Vty.Color
gruvboxRed = Vty.rgbColor (251 :: Int) 73 52

-- | Acento dorado @#fabd2f@ $(250, 189, 47)$ para encabezados y etiquetas.
gruvboxYellow :: Vty.Color
gruvboxYellow = Vty.rgbColor (250 :: Int) 189 47

-- | Estructura de paleta completa para Gruvbox Dark.
gruvboxPalette :: ThemePalette
gruvboxPalette = ThemePalette
  { tpBg0    = gruvboxBg0
  , tpBgHard = gruvboxBgHard
  , tpBg1    = gruvboxBg1
  , tpBg2    = gruvboxBg2
  , tpFg0    = gruvboxFg0
  , tpGreen  = gruvboxGreen
  , tpAqua   = gruvboxAqua
  , tpGray   = gruvboxGray
  , tpOrange = gruvboxOrange
  , tpRed    = gruvboxRed
  , tpYellow = gruvboxYellow
  }

-- | Mapa de atributos de Brick para Gruvbox Dark.
gruvboxAttrMap :: AttrMap
gruvboxAttrMap = buildThemeAttrMap gruvboxPalette

