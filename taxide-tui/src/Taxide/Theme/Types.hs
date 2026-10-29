{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Theme.Types
Description : Tipos algebraicos, nombres de atributos Brick y constructor genérico de paletas 24-bit TrueColor
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Define el catálogo de temas cromáticos de 24 bits ('ThemeName'), la estructura
de paleta semántica ('ThemePalette'), los identificadores de atributos de Brick
y la función pura 'buildThemeAttrMap' que proyecta cualquier paleta hacia el
mapa de estilos de la TUI.
-}

module Taxide.Theme.Types
  ( -- * Catálogo de Temas Soportados
    ThemeName (..)
  , ThemePalette (..)
  , parseThemeName
  , themeDisplayName
  , cycleTheme

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

    -- * Constructor Genérico de AttrMap
  , buildThemeAttrMap
  ) where

import Brick (AttrMap, AttrName, attrMap, attrName, fg, on)
import Control.DeepSeq (NFData)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Graphics.Vty as Vty

-- | Temas cromáticos de 24 bits (TrueColor) disponibles en @taxide-tui@.
-- Se selecciona Gruvbox Dark por defecto al iniciar, y puede cambiarse en vivo
-- mediante @:theme gruvbox@, @:theme nord@, @:theme dracula@ o @:theme catppuccin@.
data ThemeName
  = ThemeGruvbox
  -- ^ Paleta Gruvbox Dark (por defecto)
  | ThemeNord
  -- ^ Paleta Arctic Nord (Polar Night & Aurora)
  | ThemeDracula
  -- ^ Paleta Dracula Dark
  | ThemeCatppuccin
  -- ^ Paleta Catppuccin Mocha
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, NFData)

-- | Especificación semántica de una paleta de 11 colores RGB de 24 bits para la TUI.
data ThemePalette = ThemePalette
  { tpBg0    :: !Vty.Color
  -- ^ Fondo base principal de la cuadrícula y paneles
  , tpBgHard :: !Vty.Color
  -- ^ Fondo de alto contraste para encabezados, columna taxonómica y barras
  , tpBg1    :: !Vty.Color
  -- ^ Fondo de resalte suave para fila activa, modales y píldoras clicables
  , tpBg2    :: !Vty.Color
  -- ^ Fondo de la celda bajo el cursor activo
  , tpFg0    :: !Vty.Color
  -- ^ Color de texto primario
  , tpGreen  :: !Vty.Color
  -- ^ Estado morfológico aplicable válido
  , tpAqua   :: !Vty.Color
  -- ^ Valor implícito o heredado desde variante (@#+@)
  , tpGray   :: !Vty.Color
  -- ^ Pseudo-valor biológicamente inaplicable (@-@)
  , tpOrange :: !Vty.Color
  -- ^ Incertidumbre (@U@/@V@) o advertencia del linter
  , tpRed    :: !Vty.Color
  -- ^ Violación biológica o matemática del linter (@TAX-DEP-002@, @TAX-MATH-001@)
  , tpYellow :: !Vty.Color
  -- ^ Color de acento para encabezados, etiquetas y bordes modales
  } deriving (Eq, Show)

-- | Parsea el nombre de un tema escrito en el comando @:theme <nombre>@.
parseThemeName :: Text -> Maybe ThemeName
parseThemeName raw = case T.toLower (T.strip raw) of
  "gruvbox"          -> Just ThemeGruvbox
  "gruvbox-dark"     -> Just ThemeGruvbox
  "nord"             -> Just ThemeNord
  "dracula"          -> Just ThemeDracula
  "catppuccin"       -> Just ThemeCatppuccin
  "catppuccin-mocha" -> Just ThemeCatppuccin
  "mocha"            -> Just ThemeCatppuccin
  _                  -> Nothing

-- | Nombre legible del tema para mostrar en la barra de estado y botones.
themeDisplayName :: ThemeName -> Text
themeDisplayName ThemeGruvbox    = "Gruvbox"
themeDisplayName ThemeNord       = "Nord"
themeDisplayName ThemeDracula    = "Dracula"
themeDisplayName ThemeCatppuccin = "Catppuccin"

-- | Avanza cíclicamente al siguiente tema disponible.
cycleTheme :: ThemeName -> ThemeName
cycleTheme ThemeGruvbox    = ThemeNord
cycleTheme ThemeNord       = ThemeDracula
cycleTheme ThemeDracula    = ThemeCatppuccin
cycleTheme ThemeCatppuccin = ThemeGruvbox

attrBase :: AttrName
attrBase = attrName "taxide.base"

attrHeader :: AttrName
attrHeader = attrName "taxide.header"

attrTaxonCol :: AttrName
attrTaxonCol = attrName "taxide.taxon"

attrTaxonActive :: AttrName
attrTaxonActive = attrName "taxide.taxon.active"

attrCursor :: AttrName
attrCursor = attrName "taxide.cursor"

attrCellValid :: AttrName
attrCellValid = attrName "taxide.cell.valid"

attrCellImplicit :: AttrName
attrCellImplicit = attrName "taxide.cell.implicit"

attrCellInapplicable :: AttrName
attrCellInapplicable = attrName "taxide.cell.inapplicable"

attrCellUnknown :: AttrName
attrCellUnknown = attrName "taxide.cell.unknown"

attrCellWarning :: AttrName
attrCellWarning = attrName "taxide.cell.warning"

attrCellError :: AttrName
attrCellError = attrName "taxide.cell.error"

attrStatusNormal :: AttrName
attrStatusNormal = attrName "taxide.status.normal"

attrStatusEdit :: AttrName
attrStatusEdit = attrName "taxide.status.edit"

attrStatusCommand :: AttrName
attrStatusCommand = attrName "taxide.status.command"

attrStatusBar :: AttrName
attrStatusBar = attrName "taxide.status.bar"

attrModalBorder :: AttrName
attrModalBorder = attrName "taxide.modal.border"

attrButton :: AttrName
attrButton = attrName "taxide.button"

attrButtonActive :: AttrName
attrButtonActive = attrName "taxide.button.active"

attrButtonDanger :: AttrName
attrButtonDanger = attrName "taxide.button.danger"

-- | Construye un 'AttrMap' completo de Brick a partir de cualquier 'ThemePalette'.
buildThemeAttrMap :: ThemePalette -> AttrMap
buildThemeAttrMap p = attrMap (tpFg0 p `on` tpBg0 p)
  [ (attrBase,             tpFg0 p `on` tpBg0 p)
  , (attrHeader,           Vty.withStyle (tpYellow p `on` tpBgHard p) Vty.bold)
  , (attrTaxonCol,         tpFg0 p `on` tpBgHard p)
  , (attrTaxonActive,      Vty.withStyle (tpYellow p `on` tpBg1 p) Vty.bold)
  , (attrCursor,           Vty.withStyle (tpFg0 p `on` tpBg2 p) Vty.bold)
  , (attrCellValid,        fg (tpGreen p))
  , (attrCellImplicit,     fg (tpAqua p))
  , (attrCellInapplicable, fg (tpGray p))
  , (attrCellUnknown,      fg (tpOrange p))
  , (attrCellWarning,      Vty.withStyle (fg (tpOrange p)) Vty.bold)
  , (attrCellError,        Vty.withStyle (fg (tpRed p)) Vty.bold)
  , (attrStatusNormal,     Vty.withStyle (tpBgHard p `on` tpGreen p) Vty.bold)
  , (attrStatusEdit,       Vty.withStyle (tpBgHard p `on` tpOrange p) Vty.bold)
  , (attrStatusCommand,    Vty.withStyle (tpBgHard p `on` tpYellow p) Vty.bold)
  , (attrStatusBar,        tpFg0 p `on` tpBgHard p)
  , (attrModalBorder,      Vty.withStyle (tpYellow p `on` tpBg1 p) Vty.bold)
  , (attrButton,           Vty.withStyle (tpAqua p `on` tpBg1 p) Vty.bold)
  , (attrButtonActive,     Vty.withStyle (tpBgHard p `on` tpGreen p) Vty.bold)
  , (attrButtonDanger,     Vty.withStyle (tpFg0 p `on` tpRed p) Vty.bold)
  ]

