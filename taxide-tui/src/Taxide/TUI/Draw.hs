{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.TUI.Draw
Description : Renderizador declarativo Brick responsivo (100% pantalla), interactivo por ratón y Gruvbox 24-bit
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Implementa la función de proyección visual declarativa sobre 'Brick.Widget', respetando:
  1. El invariante de virtualización del viewport $\mathcal{V}(r_0, c_0, H_{\text{rows}}, W_{\text{cols}})$
     de la Sección 8.2 en tiempo $O(H_{\text{rows}} \cdot W_{\text{cols}})$.
  2. Adaptación responsiva al 100% del ancho y alto de la terminal ('padRight Max', 'padBottom Max'
     y anchos de columna elásticos).
  3. Soporte completo de selección y edición por ratón ('Brick.clickable') en pestañas, barra de
     herramientas, celdas de la matriz, píldoras rápidas de estado y diagnósticos del Linter.
-}

module Taxide.TUI.Draw
  ( drawTUI
  , cellAttrAndText
  , renderOriginLabel
  ) where

import Brick
  ( AttrName
  , Padding (..)
  , ViewportType (Vertical)
  , Widget
  , clickable
  , hBox
  , hLimit
  , padBottom
  , padLeft
  , padLeftRight
  , padRight
  , txt
  , vBox
  , vLimit
  , viewport
  , withAttr
  , withBorderStyle
  )
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (centerLayer)
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Taxide.Core.Character (CharacterDef (..))
import Taxide.Core.Item (ItemType (..))
import Taxide.Core.Types (PseudoValue (..), RawAttribute (..), RawStateExpr (..))
import Taxide.Delta.Serializer.Items (renderAttribute)
import Taxide.Engine.Diagnostic
  ( DiagnosticLocation (..)
  , LintDiagnostic (..)
  , Severity (..)
  , renderDiagnosticTerminal
  )
import Taxide.Engine.Variants (AttrOrigin (..), AttributedState (..), ResolvedTaxon (..))
import Taxide.TUI.State
import Taxide.TUI.Theme

-- | Renderiza la escena completa de la interfaz TUI (con capas modales para edición o ayuda).
drawTUI :: AppState -> [Widget ResourceName]
drawTUI st =
  let baseLayer = withAttr attrBase $ vBox
        [ drawTopTabsBar st
        , drawQuickActionBar st
        , drawMainPanel st
        , drawInspectorPanel st
        , drawStatusBar st
        ]
      withEdit = case asMode st of
        ModeEdit buf -> drawEditModal st buf : [baseLayer]
        _            -> [baseLayer]
  in if asShowHelp st
       then drawHelpModal : withEdit
       else withEdit

-- | Barra superior de pestañas clicables e indicadores globales del dataset.
drawTopTabsBar :: AppState -> Widget ResourceName
drawTopTabsBar st =
  let dirtyMark = if asDirty st then " [MODIFICADO *]" else ""
      errCount  = length [() | d <- asDiagnostics st, diagSeverity d == SeverityError]
      warnCount = length [() | d <- asDiagnostics st, diagSeverity d == SeverityWarning]
      tabWidget panel label =
        let active = asFocus st == panel
            attr   = if active then attrStatusCommand else attrStatusBar
        in clickable (ClickTab panel) $
             withAttr attr (txt (" " <> label <> " "))
      linterLabel = "2:Linter (E:" <> T.pack (show errCount) <> " W:" <> T.pack (show warnCount) <> ")"
      dimsLabel   = " [" <> T.pack (show (asTermWidth st)) <> "x" <> T.pack (show (asTermHeight st)) <> "] "
  in withAttr attrStatusBar $ hBox
       [ withAttr attrHeader (txt (" TAXIDE v1.0.0" <> dirtyMark <> " "))
       , txt "│ "
       , tabWidget FocusGrid   "1:Matriz 2D"
       , txt " "
       , tabWidget FocusLinter linterLabel
       , txt " "
       , tabWidget FocusTree   "3:Árbol Filogenético"
       , txt " "
       , tabWidget FocusKey    "4:Clave Dicotómica"
       , padLeft Max (txt (" [Click/Tab: Vista │ ?: Ayuda]" <> dimsLabel))
       ]

-- | Barra de herramientas interactiva con botones clicables por ratón para operaciones frecuentes.
drawQuickActionBar :: AppState -> Widget ResourceName
drawQuickActionBar st =
  let mkBtn act attr lbl =
        clickable (ClickQuickAction act) $
          withAttr attr (txt (" " <> lbl <> " "))
      themeLbl = "Tema: " <> themeDisplayName (asTheme st)
  in withAttr attrStatusBar $ hBox
       [ withAttr attrHeader (txt " Acciones: ")
       , mkBtn ActAddSpecies    attrButton       "+ Especie (o)"
       , txt " "
       , mkBtn ActAddVariant    attrButton       "+ Variante (O)"
       , txt " "
       , mkBtn ActEditCell      attrButtonActive "Editar Celda (i)"
       , txt " "
       , mkBtn ActSave          attrButton       "Guardar (:w)"
       , txt " "
       , mkBtn ActTreeParsimony attrButton       "Árbol Parsimonia"
       , txt " "
       , mkBtn ActTreeNJ        attrButton       "Árbol NJ"
       , txt " "
       , mkBtn ActKeyEs         attrButton       "Clave (:key)"
       , txt " "
       , mkBtn ActCycleTheme    attrButton       themeLbl
       , txt " "
       , mkBtn ActToggleHelp    attrButton       "? Ayuda"
       , txt " "
       , padRight Max (mkBtn ActQuit attrButtonDanger "Salir (:q)")
       ]

-- | Despacha el panel central según la vista activa $\nu \in \{\textsc{Grid}, \textsc{Linter}, \textsc{Tree}, \textsc{Key}\}$.
drawMainPanel :: AppState -> Widget ResourceName
drawMainPanel st = case asFocus st of
  FocusGrid   -> drawVirtualizedGrid st
  FocusLinter -> drawLinterView st
  FocusTree   -> drawTreeView st
  FocusKey    -> drawKeyView st

-- | Renderiza la cuadrícula morfológica 2D acotada estrictamente al viewport
-- $\mathcal{V}(r_0, c_0, H_{\text{rows}}, W_{\text{cols}})$ en tiempo $O(H_{\text{rows}} \cdot W_{\text{cols}})$,
-- expandiéndose al 100% del ancho y alto disponibles de la terminal.
drawVirtualizedGrid :: AppState -> Widget ResourceName
drawVirtualizedGrid st =
  let ((r0, r1), (c0, c1)) = visibleViewportBounds st
      nTaxa    = numTaxaState st
      nChars   = numCharsState st
      tColW    = responsiveTaxonColWidth st
      cColW    = responsiveCharColWidth st
      visibleCols = if nChars > 0 then [c0 .. c1] else []
      visibleRows = if nTaxa  > 0 then [r0 .. r1] else []
      vpLabel =
        " Matriz Morfológica Virtualizada — Filas ["
        <> T.pack (show (r0 + 1)) <> ".." <> T.pack (show (r1 + 1)) <> "/" <> T.pack (show nTaxa)
        <> "] × Caracteres [C" <> T.pack (show c0) <> "..C" <> T.pack (show c1)
        <> "/" <> T.pack (show nChars) <> "] (Click celda: seleccionar | 2º Click / Click Der.: editar) "
      headerRow = hBox $
        [ hLimit tColW $ withAttr attrHeader $
            padRight Max (txt " Taxón \\ Carácter")
        ] ++
        [ clickable (ClickCharCol cid) $
            hLimit cColW $
              let isCurCol = cid == asCursorCol st
                  hdrAttr  = if isCurCol then attrTaxonActive else attrHeader
                  colLabel = formatCharHeader cColW cid
              in withAttr hdrAttr (padRight Max (txt colLabel))
        | cid <- visibleCols
        ] ++
        [ withAttr attrHeader (padRight Max (txt " ")) ]
      dataRows = map (drawGridRow st tColW cColW visibleCols) visibleRows
      filler   = clickable MainViewport (padBottom Max (padRight Max (txt " ")))
  in withBorderStyle unicodeRounded $
       borderWithLabel (withAttr attrHeader (txt vpLabel)) $
         vBox (headerRow : hBorder : dataRows ++ [filler])
  where
    formatCharHeader colW cid =
      case Map.lookup cid (asCharMap st) of
        Nothing -> T.take (colW - 1) (" C" <> T.pack (show cid))
        Just cd ->
          let base = " C" <> T.pack (show cid) <> ":" <> T.pack (show (charType cd))
              feat = if colW >= 16 then " " <> charFeature cd else ""
          in T.take (colW - 1) (base <> feat)

-- | Renderiza una única fila visible dentro del viewport con soporte de clic por celda.
drawGridRow :: AppState -> Int -> Int -> [Int] -> Int -> Widget ResourceName
drawGridRow st tColW cColW visibleCols rowIdx =
  let isCurRow = rowIdx == asCursorRow st
      taxonLabel = case lookupTaxonAtRow rowIdx st of
        Nothing -> " (vacío)"
        Just rt ->
          let pfx = case rtType rt of
                PrimaryItem -> "#"
                VariantItem -> "#+"
              idxStr = T.pack (show (rowIdx + 1))
          in " " <> idxStr <> pfx <> " " <> rtName rt
      tAttr = if isCurRow then attrTaxonActive else attrTaxonCol
      taxonCell =
        clickable (ClickTaxonRow rowIdx) $
          hLimit tColW $ withAttr tAttr $
            padRight Max (txt (T.take (tColW - 1) taxonLabel))
      valueCells =
        [ drawGridCell st cColW rowIdx cid
        | cid <- visibleCols
        ]
      rowHighlightAttr = if isCurRow then attrTaxonActive else attrBase
  in hBox (taxonCell : valueCells ++ [withAttr rowHighlightAttr (padRight Max (txt " "))])

-- | Renderiza una celda individual $(r, c)$ clicable con su atributo cromático Gruvbox Dark.
drawGridCell :: AppState -> Int -> Int -> Int -> Widget ResourceName
drawGridCell st cColW rowIdx cid =
  let isCursor = rowIdx == asCursorRow st && cid == asCursorCol st
      (semanticAttr, cellText) = cellAttrAndText rowIdx cid st
      finalAttr = if isCursor then attrCursor else semanticAttr
      enrichedText = enrichCellDisplay cColW cid cellText st
      formatted =
        if isCursor
          then "[" <> T.take (cColW - 3) enrichedText <> "]"
          else " " <> T.take (cColW - 2) enrichedText
  in clickable (ClickCell rowIdx cid) $
       hLimit cColW $ withAttr finalAttr $ padRight Max (txt formatted)

-- | Cuando la columna responsiva es suficientemente ancha ($\ge 16$ caracteres),
-- muestra también el nombre del estado o las unidades junto al código numérico.
enrichCellDisplay :: Int -> Int -> Text -> AppState -> Text
enrichCellDisplay cColW cid baseTxt st
  | cColW < 16 = baseTxt
  | otherwise =
      case Map.lookup cid (asCharMap st) of
        Nothing -> baseTxt
        Just cd ->
          case reads (T.unpack baseTxt) :: [(Int, String)] of
            [(sid, "")] ->
              case lookup (fromIntegral sid) (charStates cd) of
                Just sName -> baseTxt <> ":" <> sName
                Nothing    -> baseTxt
            _ -> case charUnits cd of
              Just u | baseTxt /= "-" && baseTxt /= "U" -> baseTxt <> " " <> u
              _ -> baseTxt

-- | Función pura que determina el atributo cromático Gruvbox Dark y el texto resumido
-- de la celda $(r, c)$ según su estado algebraico $(\tau_{rc}, \beta_{rc})$, su origen
-- semántico y los diagnósticos activos $\Lambda_{rc}$ (Sección 8.4).
cellAttrAndText :: Int -> Int -> AppState -> (AttrName, Text)
cellAttrAndText rowIdx cid st =
  let cellDiags = lookupCellDiagnostics rowIdx cid st
      hasError  = any (\d -> diagSeverity d == SeverityError) cellDiags
      hasWarn   = any (\d -> diagSeverity d == SeverityWarning) cellDiags
      mAttrSt   = lookupCellAttributedState rowIdx cid st
  in case mAttrSt of
       Nothing ->
         let attr = if hasError then attrCellError
                    else if hasWarn then attrCellWarning
                    else attrCellUnknown
         in (attr, "U")
       Just (AttributedState raw orig) ->
         let baseTxt = case orig of
               OriginInapplicableDependency _ -> "-"
               _ -> case raw of
                 AttrText _ t -> "\"" <> T.take 16 t <> "\""
                 _            -> renderAttribute raw
             baseAttr
               | hasError  = attrCellError
               | hasWarn   = attrCellWarning
               | otherwise = classifyCellAttr raw orig
         in (baseAttr, baseTxt)

classifyCellAttr :: RawAttribute -> AttrOrigin -> AttrName
classifyCellAttr raw orig = case orig of
  OriginInapplicableDependency _ -> attrCellInapplicable
  OriginImplicitOmitted _        -> attrCellImplicit
  OriginImplicitUnspecified _    -> attrCellImplicit
  OriginInherited _              -> attrCellImplicit
  OriginExplicit -> case raw of
    AttrStates _ (PseudoState Inapplicable _) -> attrCellInapplicable
    AttrStates _ (PseudoState Unknown _)      -> attrCellUnknown
    AttrStates _ (PseudoState Variable _)     -> attrCellUnknown
    _                                         -> attrCellValid

-- | Inspector inferior de celda bajo el cursor con píldoras clicables de asignación rápida
-- de estado y alertas del Linter en vivo, ocupando el 100% del ancho de la pantalla.
drawInspectorPanel :: AppState -> Widget ResourceName
drawInspectorPanel st =
  let row = asCursorRow st
      col = asCursorCol st
      taxonTxt = case currentTaxon st of
        Nothing -> "Sin taxón"
        Just rt ->
          rtName rt <> maybe "" (\a -> " <" <> a <> ">") (rtAuthority rt)
      charTxt = case currentCharacter st of
        Nothing -> "Carácter #" <> T.pack (show col)
        Just cd ->
          "#" <> T.pack (show (charNumber cd)) <> " [" <> T.pack (show (charType cd)) <> "] "
          <> charFeature cd
          <> maybe "" (\u -> " (" <> u <> ")") (charUnits cd)
      (valTxt, origTxt) = case lookupCellAttributedState row col st of
        Nothing -> ("No codificado (U)", "Omitido")
        Just (AttributedState raw orig) ->
          (renderAttribute raw, renderOriginLabel orig)
      quickStateBar = drawQuickStatePills st valTxt
      cellDiags = lookupCellDiagnostics row col st
      diagWidget = case cellDiags of
        [] ->
          withAttr attrCellValid $
            padRight Max (txt " [✓] Sin violaciones ontológicas en esta celda. (Teclas rápidas: '-' Inaplicable | 'u' Desconocido | 'x'/Del Restaurar | 'i' Editar)")
        (d : _) ->
          let dAttr = case diagSeverity d of
                SeverityError   -> attrCellError
                SeverityWarning -> attrCellWarning
                SeverityInfo    -> attrCellImplicit
          in withAttr dAttr (padRight Max (txt (" " <> renderDiagnosticTerminal d)))
  in withBorderStyle unicodeRounded $
       borderWithLabel (withAttr attrHeader (txt " Inspector Semántico, Asignación por Ratón & Linter en Vivo ")) $
         vLimit 5 $ vBox
           [ hBox
               [ withAttr attrHeader (txt " Taxón: ")
               , txt (taxonTxt <> "   │   ")
               , withAttr attrHeader (txt "Carácter: ")
               , padRight Max (txt charTxt)
               ]
           , hBox
               [ withAttr attrHeader (txt " Valor Resuelto: ")
               , withAttr attrCellValid (txt (valTxt <> "   │   "))
               , withAttr attrHeader (txt "Origen: ")
               , withAttr attrCellImplicit (padRight Max (txt origTxt))
               ]
           , quickStateBar
           , diagWidget
           ]

-- | Genera una fila de píldoras clicables por ratón para asignar directamente estados
-- discretos o pseudo-valores ('-', 'U') a la celda activa sin necesidad de usar el teclado.
drawQuickStatePills :: AppState -> Text -> Widget ResourceName
drawQuickStatePills st currentVal =
  let statePills = case currentCharacter st of
        Nothing -> []
        Just cd ->
          [ let codeTxt = T.pack (show sid)
                isSelected = currentVal == codeTxt
                pillAttr = if isSelected then attrButtonActive else attrButton
                pillLabel = " [" <> codeTxt <> ":" <> T.take 16 sName <> "] "
            in clickable (ClickSetState codeTxt) (withAttr pillAttr (txt pillLabel))
          | (sid, sName) <- take 6 (charStates cd)
          ]
      inappPill =
        let attr = if currentVal == "-" then attrButtonActive else attrButton
        in clickable (ClickSetState "-") (withAttr attr (txt " [-:Inaplicable] "))
      unkPill =
        let attr = if currentVal == "U" then attrButtonActive else attrButton
        in clickable (ClickSetState "U") (withAttr attr (txt " [U:Desconocido] "))
      editPill =
        clickable (ClickQuickAction ActEditCell) $
          withAttr attrHeader (txt " [Editar expresión DELTA (i)] ")
      spacedPills = foldr (\w acc -> w : txt " " : acc) [inappPill, txt " ", unkPill, txt " ", editPill] statePills
  in hBox (withAttr attrHeader (txt " Click Estado: ") : spacedPills ++ [padRight Max (txt " ")])

-- | Etiqueta legible del origen semántico de un atributo resuelto.
renderOriginLabel :: AttrOrigin -> Text
renderOriginLabel orig = case orig of
  OriginExplicit ->
    "Explícito en ITEMS"
  OriginInherited parent ->
    "Heredado de '# " <> parent <> "'"
  OriginImplicitOmitted sid ->
    "Implícito (*IMPLICIT VALUES omitido -> " <> T.pack (show sid) <> ")"
  OriginImplicitUnspecified sid ->
    "Implícito (*IMPLICIT VALUES no esp. -> " <> T.pack (show sid) <> ")"
  OriginInapplicableDependency ctrl ->
    "Inaplicable por DAG (*DEPENDENT CHARACTERS controlador #" <> T.pack (show ctrl) <> ")"

-- | Vista dedicada al Inspector de Diagnósticos Linter ($\nu = \textsc{Linter}$),
-- con filas clicables para saltar directamente a la celda infractora y soporte de scroll vertical.
drawLinterView :: AppState -> Widget ResourceName
drawLinterView st =
  let diags = asDiagnostics st
      taxaVec = asResolvedTaxa st
      findTaxonRow tName =
        findIndex (\rt -> rtName rt == tName) (V.toList taxaVec)
      rows = if null diags
        then [withAttr attrCellValid (padRight Max (txt " [✓] 0 violaciones detectadas en la matriz morfológica."))]
        else
          [ let attr = case diagSeverity d of
                  SeverityError   -> attrCellError
                  SeverityWarning -> attrCellWarning
                  SeverityInfo    -> attrCellImplicit
                loc = diagLocation d
                mJump = do
                  tName <- locTaxon loc
                  rIdx  <- findTaxonRow tName
                  let cIdx = maybe 1 id (locCharId loc)
                  pure (rIdx, cIdx)
                lineW = withAttr attr (padRight Max (txt (" " <> renderDiagnosticTerminal d <> "  [Click para ir a celda]")))
            in case mJump of
                 Just (rIdx, cIdx) -> clickable (ClickDiagJump rIdx cIdx) lineW
                 Nothing           -> lineW
          | d <- diags
          ]
  in withBorderStyle unicodeRounded $
       borderWithLabel (withAttr attrHeader (txt " Auditoría Completa del Linter Ontológico y Cladístico (Click diagnóstico para saltar | j/k/Rueda: Scroll) ")) $
         clickable LinterViewport $
           viewport LinterViewport Vertical (vBox rows)

-- | Vista dedicada al Árbol Filogenético / Fenético ($\nu = \textsc{Tree}$).
drawTreeView :: AppState -> Widget ResourceName
drawTreeView st =
  let content = case asCachedTree st of
        Nothing ->
          "Haga clic en [Árbol Parsimonia] / [Árbol NJ] o ejecute ':tree parsimony', ':tree upgma' o ':tree nj'."
        Just rendered -> rendered
      linesWidgets =
        [ padRight Max (txt (" " <> l))
        | l <- T.lines content
        ]
  in withBorderStyle unicodeRounded $
       borderWithLabel (withAttr attrHeader (txt " Visor de Cladograma / Fenograma (ASCII/Unicode & Newick | j/k/PgDn/Rueda: Scroll) ")) $
         clickable TreeViewport $
           viewport TreeViewport Vertical (vBox linesWidgets)

-- | Vista dedicada a la Clave Dicotómica ($\nu = \textsc{Key}$).
drawKeyView :: AppState -> Widget ResourceName
drawKeyView st =
  let content = case asCachedKey st of
        Nothing ->
          "Haga clic en [Clave (:key)] o ejecute ':key es' o ':key en' para generar la clave dicotómica por entropía de Shannon."
        Just rendered -> rendered
      linesWidgets =
        [ padRight Max (txt (" " <> l))
        | l <- T.lines content
        ]
  in withBorderStyle unicodeRounded $
       borderWithLabel (withAttr attrHeader (txt " Clave Dicotómica de Identificación (Entropía de Shannon | j/k/PgDn/Rueda: Scroll) ")) $
         clickable KeyViewport $
           viewport KeyViewport Vertical (vBox linesWidgets)

-- | Ventana modal emergente en 'ModeEdit' con botones clicables de estados y confirmación.
drawEditModal :: AppState -> Text -> Widget ResourceName
drawEditModal st buf =
  let col = asCursorCol st
      tName = maybe "-" rtName (currentTaxon st)
      cDesc = maybe ("#" <> T.pack (show col))
                    (\cd -> "#" <> T.pack (show col) <> " (" <> T.pack (show (charType cd)) <> ") " <> charFeature cd)
                    (currentCharacter st)
      modalStateButtons = case currentCharacter st of
        Nothing -> txt ""
        Just cd ->
          if null (charStates cd)
            then txt ""
            else hBox $
              [ withAttr attrHeader (txt "Click directo: ") ] ++
              [ clickable (ClickSetState (T.pack (show sid))) $
                  withAttr attrButton (txt (" [" <> T.pack (show sid) <> ":" <> T.take 12 sName <> "] "))
              | (sid, sName) <- take 4 (charStates cd)
              ]
  in centerLayer $
       hLimit 76 $
         withBorderStyle unicodeRounded $
           borderWithLabel (withAttr attrModalBorder (txt " Editor Reactivo de Atributo DELTA ")) $
             padLeftRight 2 $ vBox
               [ txt ("Taxón    : " <> tName)
               , txt ("Carácter : " <> cDesc)
               , hBorder
               , hBox
                   [ withAttr attrHeader (txt "Expresión DELTA > ")
                   , withAttr attrCursor (txt (buf <> "█"))
                   ]
               , modalStateButtons
               , txt "Sintaxis: 1 | 1/2 (OR) | 1&2 (AND) | 1-3 | - (Inaplicable) | U | (5-)10-15(-20)"
               , hBorder
               , hBox
                   [ clickable ClickModalConfirm (withAttr attrButtonActive (txt " [ ✓ Confirmar (Enter) ] "))
                   , txt "   "
                   , clickable ClickModalCancel  (withAttr attrButtonDanger (txt " [ ✗ Cancelar (Esc) ] "))
                   ]
               ]

-- | Ventana modal de ayuda interactiva (@?@ o botón @[? Ayuda]@).
drawHelpModal :: Widget ResourceName
drawHelpModal =
  centerLayer $
    hLimit 84 $
      withBorderStyle unicodeRounded $
        borderWithLabel (withAttr attrModalBorder (txt " Guía Rápida de Atajos de Teclado, Ratón y Comandos de TAXIDE ")) $
          padLeftRight 2 $ vBox
            [ withAttr attrHeader (txt "1. Controles de Ratón (Mouse Tracking):")
            , txt "  • Click izquierdo en celda    : Selecciona la celda (2º click abre editor)"
            , txt "  • Click derecho en celda      : Abre directamente el editor de esa celda"
            , txt "  • Click en '[1:estado]' abajo : Asigna ese estado en 1 click y propaga el DAG"
            , txt "  • Rueda del ratón (Scroll)    : Desplaza verticalmente la matriz"
            , txt "  • Click en pestañas / botones : Cambia vista, agrega especie, guarda o exporta"
            , hBorder
            , withAttr attrHeader (txt "2. Atajos de Teclado (Modo Normal Estilo VIM):")
            , txt "  • h, j, k, l / Flechas        : Mover cursor 2D (Izquierda, Abajo, Arriba, Derecha)"
            , txt "  • g / G  |  0 / $             : Primera/última fila  |  Primera/última columna"
            , txt "  • Ctrl+d / Ctrl+u / PgDn/PgUp : Avanzar / retroceder media página"
            , txt "  • i / e / Enter               : Editar expresión DELTA de la celda activa"
            , txt "  • -  |  u  |  x / Del         : Inaplicable (-) | Desconocida (U) | Restaurar heredado/implícito"
            , txt "  • o  |  O                     : Agregar nueva especie (:add) o variante (:add+)"
            , txt "  • Tab  |  1, 2, 3, 4          : Alternar entre Matriz, Linter, Árbol y Clave"
            , txt "  • /texto                      : Buscar taxón o carácter por nombre"
            , hBorder
            , withAttr attrHeader (txt "3. Comandos Ex (pulsando ':'):")
            , txt "  • :add Nombre <Autor, Año>    : Agrega una especie primaria (#) y salta a su fila"
            , txt "  • :add+ Nombre variante       : Agrega una variante subordinada (#+)"
            , txt "  • :tree [parsimony|upgma|nj]  : Reconstruye cladograma o fenograma en vivo"
            , txt "  • :key [es|en]                : Genera clave dicotómica por entropía de Shannon"
            , txt "  • :theme <nombre>             : Cambia tema (gruvbox | nord | dracula | catppuccin)"
            , txt "  • :export <fmt> <archivo>     : Exporta a nexus|tnt|dwca|json|md|html|hdf5"
            , txt "  • :w  |  :wq  |  :q!          : Guardar cambios  |  Guardar y salir  |  Salir sin guardar"
            , hBorder
            , clickable ClickModalCancel (withAttr attrButtonActive (txt " [ Cerrar Ayuda (Esc / ? / Enter) ] "))
            ]

-- | Barra de estado modal inferior estilo VIM (100% ancho de pantalla).
drawStatusBar :: AppState -> Widget ResourceName
drawStatusBar st =
  let (modeAttr, modeLabel) = case asMode st of
        ModeNormal    -> (attrStatusNormal,  " NORMAL ")
        ModeEdit _    -> (attrStatusEdit,    " EDIT   ")
        ModeCommand _ -> (attrStatusCommand, " COMMAND")
        ModeSearch _  -> (attrStatusCommand, " SEARCH ")
      posText =
        " [T" <> T.pack (show (asCursorRow st + 1)) <> "/" <> T.pack (show (numTaxaState st))
        <> ", C" <> T.pack (show (asCursorCol st)) <> "/" <> T.pack (show (numCharsState st)) <> "] "
  in withAttr attrStatusBar $ hBox
       [ withAttr modeAttr (txt modeLabel)
       , withAttr attrHeader (txt posText)
       , padRight Max (txt (" " <> asStatusMsg st))
       ]
