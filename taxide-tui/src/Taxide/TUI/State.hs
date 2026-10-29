{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.TUI.State
Description : Estado formal Σ de la máquina Model-View-Update de TAXIDE con layout responsivo y eventos de ratón
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Materializa la séxtupla formal $\Sigma = (\mathcal{D}, \mathcal{M}, \Delta_{\text{overlay}}, \kappa, \mu, \nu)$
definida en la Sección 8.1 de la especificación matemática, junto con el cálculo de la
ventana virtualizada 2D $\mathcal{V}(r_0, c_0, H_{\text{rows}}, W_{\text{cols}})$, el
dimensionamiento responsivo de columnas según el tamaño de la terminal y los identificadores
de regiones interactivas por ratón ('ResourceName').
-}

module Taxide.TUI.State
  ( -- * Identificadores de Recursos Interactivos y Modos Modales
    QuickAction (..)
  , ResourceName (..)
  , AppMode (..)
  , FocusPanel (..)
  , ExportTargetCmd (..)
  , PendingIOAction (..)

    -- * Estado Principal de la Aplicación
  , AppState (..)
  , defaultViewportRows
  , defaultViewportCols
  , initAppState
  , recomputePipelineState

    -- * Consultas Puras sobre Celdas, Dimensiones Responsivas y Viewport
  , numTaxaState
  , numCharsState
  , currentTaxon
  , currentCharacter
  , lookupTaxonAtRow
  , lookupCellAttributedState
  , lookupCellDiagnostics
  , visibleViewportBounds
  , responsiveTaxonColWidth
  , responsiveCharColWidth
  , cycleFocusPanel
  ) where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Taxide.Core.Character (CharacterDef (..))
import Taxide.Core.Specs (SpecsDef (..))
import Taxide.Core.Types (RawAttribute)
import Taxide.Delta.Pipeline (DeltaDataset (..))
import Taxide.Engine.Diagnostic
  ( DiagnosticLocation (..)
  , LintDiagnostic (..)
  , Severity (..)
  )
import Taxide.Engine.Matrix (ColumnarMatrix (..))
import Taxide.Engine.Pipeline (runPipelinePure)
import Taxide.Engine.Variants (AttributedState (..), ResolvedTaxon (..))
import Taxide.Theme.Types (ThemeName (..))

-- | Acciones rápidas ejecutables mediante un clic de ratón en la barra de herramientas.
data QuickAction
  = ActEditCell
  -- ^ Abre el editor modal para la celda bajo el cursor (@i@)
  | ActAddSpecies
  -- ^ Inicia la adición de una nueva especie primaria (@:add @ / @o@)
  | ActAddVariant
  -- ^ Inicia la adición de una variante subordinada (@:add+ @)
  | ActSave
  -- ^ Guarda el dataset canónico en disco (@:w@)
  | ActTreeParsimony
  -- ^ Reconstruye el cladograma de máxima parsimonia (@:tree parsimony@)
  | ActTreeNJ
  -- ^ Reconstruye el fenograma Neighbor-Joining (@:tree nj@)
  | ActKeyEs
  -- ^ Genera la clave dicotómica en español (@:key es@)
  | ActCycleTheme
  -- ^ Alterna cíclicamente el tema cromático (@:theme gruvbox|nord|dracula|catppuccin@)
  | ActToggleHelp
  -- ^ Muestra/oculta el panel de ayuda de atajos y comandos (@?@)
  | ActQuit
  -- ^ Sale de la aplicación (@:q@)
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Identificadores de recursos y zonas clicables por ratón ('Brick.clickable').
data ResourceName
  = MainViewport
  | LinterViewport
  | TreeViewport
  | KeyViewport
  | ClickTab !FocusPanel
  -- ^ Clic en una pestaña superior para cambiar la vista activa
  | ClickQuickAction !QuickAction
  -- ^ Clic en un botón de la barra de herramientas rápida
  | ClickCell !Int !Int
  -- ^ Clic en la celda @(fila, carácter)@: primer clic selecciona; segundo clic o clic derecho abre edición
  | ClickTaxonRow !Int
  -- ^ Clic en el nombre del taxón en la fila @r@
  | ClickCharCol !Int
  -- ^ Clic en el encabezado de la columna de carácter @c@
  | ClickSetState !Text
  -- ^ Clic en una píldora rápida de estado (ej. @"1"@, @"2"@, @"-"@, @"U"@) para asignarlo instantáneamente
  | ClickDiagJump !Int !Int
  -- ^ Clic en un diagnóstico del Linter para saltar a la celda @(fila, carácter)@ infractora
  | ClickModalConfirm
  -- ^ Clic en el botón Confirmar del modal de edición
  | ClickModalCancel
  -- ^ Clic en el botón Cancelar del modal de edición o cerrar ayuda
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Modo de operación modal estilo VIM ($\mu$).
data AppMode
  = ModeNormal
  -- ^ Navegación 2D con @hjkl@/ratón, saltos y cambio de paneles
  | ModeEdit !Text
  -- ^ Edición en vivo de la expresión DELTA de la celda bajo el cursor
  | ModeCommand !Text
  -- ^ Línea de comandos estilo Ex (@:w@, @:q@, @:add@, @:tree@, @:key@, @:export@)
  | ModeSearch !Text
  -- ^ Búsqueda incremental de taxones o caracteres (@/@)
  deriving (Eq, Show, Generic, NFData)

-- | Vista activa proyectada en el panel principal ($\nu$).
data FocusPanel
  = FocusGrid
  -- ^ Hoja de cálculo matricial 2D virtualizada con inspector en vivo
  | FocusLinter
  -- ^ Panel completo de auditoría y diagnósticos del Linter ($\Lambda$)
  | FocusTree
  -- ^ Visor de cladograma / fenograma ASCII/Unicode y Newick
  | FocusKey
  -- ^ Visor de clave dicotómica generada por entropía de Shannon
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Formatos admitidos por el comando interactivo @:export@.
data ExportTargetCmd
  = CmdExportNexus
  | CmdExportTnt
  | CmdExportDwca
  | CmdExportJson
  | CmdExportMd
  | CmdExportHtml
  | CmdExportHdf5
  deriving (Eq, Show, Generic, NFData)

-- | Efectos de escritura en disco encolados por comandos puros (@:w@, @:export@)
-- para ser ejecutados en la frontera de IO de 'Taxide.TUI.Runner'.
data PendingIOAction
  = ActionSaveDataset !FilePath
  | ActionExport !ExportTargetCmd !FilePath
  deriving (Eq, Show, Generic, NFData)

-- | Estado estricto $\Sigma$ de la interfaz reactiva de terminal.
data AppState = AppState
  { asDatasetDir   :: !FilePath
  -- ^ Directorio base del dataset DELTA
  , asDataset      :: !DeltaDataset
  -- ^ Dataset DELTA en memoria ($\mathcal{D}$)
  , asCharMap      :: !(Map Int CharacterDef)
  -- ^ Mapa indexado de definiciones de caracteres ($1 \dots M$)
  , asResolvedTaxa :: !(Vector ResolvedTaxon)
  -- ^ Vector $0 \dots N-1$ de taxones resueltos por el pipeline
  , asMatrix       :: !ColumnarMatrix
  -- ^ Matriz columnar resuelta SoA ($\mathcal{M}$)
  , asDiagnostics  :: ![LintDiagnostic]
  -- ^ Lista consolidada de diagnósticos activos ($\Lambda$)
  , asCellDiags    :: !(Map (Text, Int) [LintDiagnostic])
  -- ^ Índice rápido `(nombreTaxón, idCarácter) -> [LintDiagnostic]`
  , asOverlay      :: !(Map (Int, Int) RawAttribute)
  -- ^ Mapa $\Delta_{\text{overlay}}$ de ediciones incrementales `(fila, carácter)`
  , asCursorRow    :: !Int
  -- ^ Fila activa del cursor $\kappa_r \in \{0, \dots, N-1\}$
  , asCursorCol    :: !Int
  -- ^ Columna activa del cursor $\kappa_c \in \{1, \dots, M\}$
  , asVpRowOffset  :: !Int
  -- ^ Desplazamiento vertical del viewport $r_0 \in \{0, \dots, N-1\}$
  , asVpColOffset  :: !Int
  -- ^ Desplazamiento horizontal del viewport $c_0 \in \{1, \dots, M\}$
  , asVpRows       :: !Int
  -- ^ Número de filas visibles en el viewport $H_{\text{rows}}$
  , asVpCols       :: !Int
  -- ^ Número de columnas de caracteres visibles en el viewport $W_{\text{cols}}$
  , asTermWidth    :: !Int
  -- ^ Ancho actual de la terminal en columnas de caracteres
  , asTermHeight   :: !Int
  -- ^ Alto actual de la terminal en filas de caracteres
  , asMode         :: !AppMode
  -- ^ Modo modal actual $\mu$
  , asFocus        :: !FocusPanel
  -- ^ Vista activa $\nu$
  , asTheme        :: !ThemeName
  -- ^ Tema cromático de 24 bits activo (por defecto 'ThemeGruvbox')
  , asShowHelp     :: !Bool
  -- ^ Indica si el modal de ayuda rápida (@?@) está desplegado
  , asDirty        :: !Bool
  -- ^ Indica si existen ediciones en memoria pendientes de guardar en disco
  , asShouldQuit   :: !Bool
  -- ^ Señal pura de terminación de sesión (@:q@, @:wq@, @:q!@)
  , asStatusMsg    :: !Text
  -- ^ Mensaje informativo o de diagnóstico en la barra de estado inferior
  , asCachedTree   :: !(Maybe Text)
  -- ^ Caché del último árbol reconstruido (@:tree@)
  , asCachedKey    :: !(Maybe Text)
  -- ^ Caché de la última clave dicotómica generada (@:key@)
  , asPendingIO    :: ![PendingIOAction]
  -- ^ Acciones de E/S pendientes de vaciar por el ejecutor en 'IO'
  } deriving (Eq, Show, Generic)

defaultViewportRows :: Int
defaultViewportRows = 18

defaultViewportCols :: Int
defaultViewportCols = 8

-- | Construye el estado inicial $\Sigma_0$ a partir de un 'DeltaDataset' ejecutando
-- inmediatamente el pipeline semántico puro $\Lambda \circ \mathcal{M} \circ \Delta \circ \mathcal{J} \circ \mathcal{V}$.
initAppState :: FilePath -> DeltaDataset -> AppState
initAppState dir ds =
  let specs = dsSpecs ds
      chars = dsChars ds
      items = dsItems ds
      (taxaList, mat, diags) = runPipelinePure specs items
      charMap = Map.fromList [(charNumber c, c) | c <- chars]
      cellDiagMap = buildCellDiagMap diags
      errCount = length [() | d <- diags, diagSeverity d == SeverityError]
      warnCount = length [() | d <- diags, diagSeverity d == SeverityWarning]
      initMsg =
        "Dataset cargado (" <> T.pack (show (length taxaList)) <> " taxones, "
        <> T.pack (show (specNumCharacters specs)) <> " caracteres) — "
        <> T.pack (show errCount) <> " errores, "
        <> T.pack (show warnCount) <> " avisos. [Click o 'i' para editar | '?' ayuda]"
  in AppState
       { asDatasetDir   = dir
       , asDataset      = ds
       , asCharMap      = charMap
       , asResolvedTaxa = V.fromList taxaList
       , asMatrix       = mat
       , asDiagnostics  = diags
       , asCellDiags    = cellDiagMap
       , asOverlay      = Map.empty
       , asCursorRow    = 0
       , asCursorCol    = 1
       , asVpRowOffset  = 0
       , asVpColOffset  = 1
       , asVpRows       = defaultViewportRows
       , asVpCols       = defaultViewportCols
       , asTermWidth    = 120
       , asTermHeight   = 36
       , asMode         = ModeNormal
       , asFocus        = FocusGrid
       , asTheme        = ThemeGruvbox
       , asShowHelp     = False
       , asDirty        = False
       , asShouldQuit   = False
       , asStatusMsg    = initMsg
       , asCachedTree   = Nothing
       , asCachedKey    = Nothing
       , asPendingIO    = []
       }

-- | Re-ejecuta el pipeline semántico en memoria tras una edición en 'asDataset',
-- propagando el DAG de '*DEPENDENT CHARACTERS' y actualizando la matriz SoA y el Linter.
recomputePipelineState :: AppState -> AppState
recomputePipelineState st =
  let ds = asDataset st
      specs = dsSpecs ds
      items = dsItems ds
      (taxaList, mat, diags) = runPipelinePure specs items
      cellDiagMap = buildCellDiagMap diags
  in st
       { asResolvedTaxa = V.fromList taxaList
       , asMatrix       = mat
       , asDiagnostics  = diags
       , asCellDiags    = cellDiagMap
       , asCachedTree   = Nothing
       , asCachedKey    = Nothing
       }

buildCellDiagMap :: [LintDiagnostic] -> Map (Text, Int) [LintDiagnostic]
buildCellDiagMap diags =
  Map.fromListWith (++)
    [ ((tName, cid), [d])
    | d <- diags
    , let loc = diagLocation d
    , Just tName <- [locTaxon loc]
    , Just cid   <- [locCharId loc]
    ]

-- | Número de taxones $N$ en el estado actual.
numTaxaState :: AppState -> Int
numTaxaState = V.length . asResolvedTaxa

-- | Número de caracteres $M$ en el estado actual.
numCharsState :: AppState -> Int
numCharsState st = max 1 (specNumCharacters (dsSpecs (asDataset st)))

-- | Devuelve el taxón resuelto en la fila dada ($0 \dots N-1$).
lookupTaxonAtRow :: Int -> AppState -> Maybe ResolvedTaxon
lookupTaxonAtRow r st
  | r >= 0 && r < V.length (asResolvedTaxa st) = Just (asResolvedTaxa st V.! r)
  | otherwise = Nothing

-- | Devuelve el taxón bajo el cursor actual.
currentTaxon :: AppState -> Maybe ResolvedTaxon
currentTaxon st = lookupTaxonAtRow (asCursorRow st) st

-- | Devuelve la definición del carácter bajo el cursor actual.
currentCharacter :: AppState -> Maybe CharacterDef
currentCharacter st = Map.lookup (asCursorCol st) (asCharMap st)

-- | Consulta el atributo resuelto y su origen semántico en la celda @(row, col)@.
lookupCellAttributedState :: Int -> Int -> AppState -> Maybe AttributedState
lookupCellAttributedState row col st = do
  taxon <- lookupTaxonAtRow row st
  Map.lookup col (rtAttributes taxon)

-- | Consulta los diagnósticos del Linter asociados específicamente a la celda @(row, col)@.
lookupCellDiagnostics :: Int -> Int -> AppState -> [LintDiagnostic]
lookupCellDiagnostics row col st =
  case lookupTaxonAtRow row st of
    Nothing -> []
    Just taxon ->
      Map.findWithDefault [] (rtName taxon, col) (asCellDiags st)

-- | Calcula el ancho responsivo de la columna de nombres de taxón según el ancho de la terminal.
responsiveTaxonColWidth :: AppState -> Int
responsiveTaxonColWidth st =
  let w = asTermWidth st
  in max 26 (min 44 (w `div` 4))

-- | Calcula el ancho responsivo de cada columna de carácter para aprovechar pantallas anchas.
responsiveCharColWidth :: AppState -> Int
responsiveCharColWidth st =
  let w      = asTermWidth st
      tColW  = responsiveTaxonColWidth st
      availW = max 24 (w - tColW - 4)
      nChars = max 1 (min (numCharsState st) (asVpCols st))
  in max 12 (min 26 (availW `div` nChars))

-- | Calcula los límites visibles de la ventana virtualizada 2D:
-- $\mathcal{V}(r_0, c_0, H_{\text{rows}}, W_{\text{cols}}) = [r_0, \min(N-1, r_0+H-1)] \times [c_0, \min(M, c_0+W-1)]$.
visibleViewportBounds :: AppState -> ((Int, Int), (Int, Int))
visibleViewportBounds st =
  let nTaxa  = numTaxaState st
      nChars = numCharsState st
      r0     = max 0 (min (max 0 (nTaxa - 1)) (asVpRowOffset st))
      r1     = max r0 (min (max 0 (nTaxa - 1)) (r0 + max 1 (asVpRows st) - 1))
      c0     = max 1 (min nChars (asVpColOffset st))
      c1     = max c0 (min nChars (c0 + max 1 (asVpCols st) - 1))
  in ((r0, r1), (c0, c1))

-- | Alterna cíclicamente entre las cuatro vistas principales con @Tab@.
cycleFocusPanel :: FocusPanel -> FocusPanel
cycleFocusPanel FocusGrid   = FocusLinter
cycleFocusPanel FocusLinter = FocusTree
cycleFocusPanel FocusTree   = FocusKey
cycleFocusPanel FocusKey    = FocusGrid
