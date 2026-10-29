{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.TUI.Theme
Description : Fachada unificada de temas 24-bit TrueColor (Gruvbox, Nord, Dracula, Catppuccin)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Orquesta los módulos de paleta separados en @Taxide.Theme.*@ ('Taxide.Theme.Gruvbox',
'Taxide.Theme.Nord', 'Taxide.Theme.Dracula', 'Taxide.Theme.Catppuccin'), arrancando con
Gruvbox Dark por defecto y permitiendo el cambio dinámico en tiempo de ejecución mediante
los comandos @:theme gruvbox@, @:theme nord@, @:theme dracula@ y @:theme catppuccin@.
-}

module Taxide.TUI.Theme
  ( -- * Selección Dinámica de Temas
    ThemeName (..)
  , ThemePalette (..)
  , parseThemeName
  , themeDisplayName
  , cycleTheme
  , themePalette
  , themeAttrMap
  , taxideAttrMap

    -- * Nombres de Atributos de Brick
  , attrBase
  , attrHeader
  , attrTaxonCol
  , attrTaxonActive
  , attrCursor
  , attrCellValid
  , attrCellImplicit
  , attrCellInapplicable
  , attrCellUnknown
  , attrCellWarning
  , attrCellError
  , attrStatusNormal
  , attrStatusEdit
  , attrStatusCommand
  , attrStatusBar
  , attrModalBorder
  , attrButton
  , attrButtonActive
  , attrButtonDanger

    -- * Reexportación de Paletas Individuales
  , module Taxide.Theme.Gruvbox
  , module Taxide.Theme.Nord
  , module Taxide.Theme.Dracula
  , module Taxide.Theme.Catppuccin
  ) where

import Brick (AttrMap)

import Taxide.Theme.Catppuccin
import Taxide.Theme.Dracula
import Taxide.Theme.Gruvbox
import Taxide.Theme.Nord
import Taxide.Theme.Types

-- | Obtiene la estructura 'ThemePalette' correspondiente al tema seleccionado.
themePalette :: ThemeName -> ThemePalette
themePalette ThemeGruvbox    = gruvboxPalette
themePalette ThemeNord       = nordPalette
themePalette ThemeDracula    = draculaPalette
themePalette ThemeCatppuccin = catppuccinPalette

-- | Obtiene el 'AttrMap' de Brick para el tema activo en el estado de la TUI.
themeAttrMap :: ThemeName -> AttrMap
themeAttrMap ThemeGruvbox    = gruvboxAttrMap
themeAttrMap ThemeNord       = nordAttrMap
themeAttrMap ThemeDracula    = draculaAttrMap
themeAttrMap ThemeCatppuccin = catppuccinAttrMap

-- | Mapa de atributos por defecto (Gruvbox Dark de 24 bits).
taxideAttrMap :: AttrMap
taxideAttrMap = gruvboxAttrMap
