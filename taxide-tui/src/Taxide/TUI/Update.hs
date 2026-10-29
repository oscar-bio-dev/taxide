{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.TUI.Update
Description : Endomorfismo puro de transición de eventos τ (teclado, ratón y redimensionado) y propagación reactiva
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Implementa la función de transición modal pura $\tau \colon \Sigma \times \mathrm{Event}_{\text{Vty}} \to \Sigma$
definida en la Sección 8.1, el manejador puro de eventos de ratón ('stepTUIMouse'), la
adaptación responsiva de dimensiones ('applyTerminalResize'), la actualización del viewport
virtualizado 2D de la Sección 8.2 y la propagación reactiva incremental del DAG y del Linter
en vivo de la Sección 8.3.
-}

module Taxide.TUI.Update
  ( -- * Transiciones Puras de Eventos (Headless-Testable)
    stepTUIEvent
  , stepTUIEvents
  , stepTUIMouse
  , applyTerminalResize
  , executeCommandPure
  , applyCellEditText

    -- * Manejador Monádico para Brick
  , handleAppEvent
  , flushPendingIOActions
  ) where

import Brick (BrickEvent (..), EventM, get, halt, put)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Text.Megaparsec (eof, runParser)

import Taxide.Core.Character (CharacterDef (..))
import Taxide.Core.Invariants (collectActiveStates, isDependencyTriggered)
import Taxide.Core.Item (ItemDef (..), ItemType (..))
import Taxide.Core.Specs (DependentRule (..), SpecsDef (..))
import Taxide.Core.Types
  ( DatasetMetadata
  , PseudoValue (..)
  , RawAttribute (..)
  , RawStateExpr (..)
  , emptyDatasetMetadata
  )
import Taxide.Delta.Parser.Items (pAttribute)
import Taxide.Delta.Parser.Lexer (sc)
import Taxide.Delta.Pipeline (DeltaDataset (..), writeDeltaDataset)
import Taxide.Delta.Serializer.Items (renderAttribute)
import Taxide.Engine.Diagnostic (LintDiagnostic (..), Severity (..))
import Taxide.Engine.Discretize (DiscretizeMethod (..), DiscretizeTarget (..))
import Taxide.Engine.Gower (buildNeighborJoining, buildUPGMA, computeGowerMatrix)
import Taxide.Engine.Key
  ( KeyLang (..)
  , buildIdentificationKey
  , defaultKeyOptions
  , renderBracketedKey
  )
import Taxide.Engine.Matrix (ColumnarMatrix (..))
import Taxide.Engine.Sankoff (TreeEvaluation (..), evaluateTreeParsimony)
import Taxide.Engine.Search (SearchResult (..), SearchStrategy (..), runParsimonySearch)
import Taxide.Engine.Tree (mapTreeTaxa, renderAsciiTree, renderNewick)
import Taxide.Engine.Variants (AttrOrigin (..), AttributedState (..), ResolvedTaxon (..))
import Taxide.Export.BinaryHPC (renderHpcBinary)
import Taxide.Export.Common (mkExportContext, renderExportError)
import Taxide.Export.DarwinCore (DwcOptions (..), renderDwcArchive)
import Taxide.Export.Json (renderJson)
import Taxide.Export.Natural
  ( defaultNaturalOptions
  , renderNaturalHtml
  , renderNaturalMarkdown
  )
import Taxide.Export.Nexus (NexusOptions (..), renderNexus)
import Taxide.Export.TNT (TntOptions (..), renderTnt)
import Taxide.Theme.Types (cycleTheme, parseThemeName, themeDisplayName)
import Taxide.TUI.State

-- | Aplica una secuencia de eventos Vty de forma puramente funcional y estricta.
stepTUIEvents :: AppState -> [Vty.Event] -> AppState
stepTUIEvents = foldl stepTUIEvent

-- | Recalcula las dimensiones responsivas del viewport ante un cambio de tamaño de la terminal $(W, H)$.
applyTerminalResize :: Int -> Int -> AppState -> AppState
applyTerminalResize w h st =
  let safeW     = max 60 w
      safeH     = max 18 h
      -- Reservamos filas para: barra de pestañas (1) + barra de acciones (1) + bordes y cabecera (4)
      -- + inspector semántico (8) + barra de estado (1) = 15 filas fijas.
      newVpRows = max 4 (safeH - 15)
      tColW     = max 26 (min 44 (safeW `div` 4))
      availW    = max 24 (safeW - tColW - 4)
      newVpCols = max 2 (availW `div` 14)
  in adjustViewport $ st
       { asTermWidth  = safeW
       , asTermHeight = safeH
       , asVpRows     = newVpRows
       , asVpCols     = newVpCols
       }

-- | Endomorfismo puro de transición de eventos de teclado y redimensionado:
-- $\tau \colon \Sigma \times \mathrm{Event}_{\text{Vty}} \longrightarrow \Sigma$.
stepTUIEvent :: AppState -> Vty.Event -> AppState
stepTUIEvent st ev = case ev of
  Vty.EvResize w h ->
    applyTerminalResize w h st
  _ | asShowHelp st ->
    case ev of
      Vty.EvKey Vty.KEsc        [] -> st { asShowHelp = False }
      Vty.EvKey (Vty.KChar '?') [] -> st { asShowHelp = False }
      Vty.EvKey (Vty.KChar 'q') [] -> st { asShowHelp = False }
      Vty.EvKey Vty.KEnter      [] -> st { asShowHelp = False }
      _                            -> st
  _ -> case asMode st of
    ModeNormal      -> stepNormalMode st ev
    ModeEdit buf    -> stepEditMode st buf ev
    ModeCommand cmd -> stepCommandMode st cmd ev
    ModeSearch qry  -> stepSearchMode st qry ev

-- | Manejador puramente funcional de eventos de ratón ('MouseDown') sobre zonas clicables.
stepTUIMouse :: AppState -> ResourceName -> Vty.Button -> [Vty.Modifier] -> AppState
stepTUIMouse st rn btn _mods = case btn of
  -- Rueda del ratón: desplaza el cursor verticalmente
  Vty.BScrollUp   -> moveCursor (-1) 0 st
  Vty.BScrollDown -> moveCursor 1 0 st

  -- Clic derecho sobre cualquier celda abre inmediatamente su editor modal
  Vty.BRight -> case rn of
    ClickCell r c ->
      enterEditMode (setCursorPos r c (st { asFocus = FocusGrid, asShowHelp = False }))
    _ -> st

  -- Clic izquierdo sobre pestañas, botones, celdas, estados o diagnósticos
  Vty.BLeft -> case rn of
    ClickTab panel ->
      switchFocus panel (st { asShowHelp = False })

    ClickQuickAction act ->
      runQuickAction act st

    ClickCell r c ->
      let stFocused = st { asFocus = FocusGrid, asShowHelp = False }
      in if asCursorRow stFocused == r && asCursorCol stFocused == c && asMode stFocused == ModeNormal
           then enterEditMode stFocused
           else
             let stMoved = setCursorPos r c stFocused
             in stMoved
                  { asStatusMsg =
                      "Celda seleccionada (T" <> T.pack (show (r + 1)) <> ", C" <> T.pack (show c)
                      <> "). Vuelva a hacer clic, clic derecho o pulse 'i' para editar."
                  }

    ClickTaxonRow r ->
      setCursorPos r (asCursorCol st) (st { asFocus = FocusGrid, asShowHelp = False })

    ClickCharCol c ->
      setCursorPos (asCursorRow st) c (st { asFocus = FocusGrid, asShowHelp = False })

    ClickSetState stateExpr ->
      applyCellEditText stateExpr (st { asShowHelp = False })

    ClickDiagJump r c ->
      let stJumped = setCursorPos r c (st { asFocus = FocusGrid, asShowHelp = False })
      in stJumped
           { asStatusMsg =
               "Saltando a celda con diagnóstico (T" <> T.pack (show (r + 1))
               <> ", C" <> T.pack (show c) <> ")."
           }

    ClickModalConfirm ->
      case asMode st of
        ModeEdit buf -> stepEditMode st buf (Vty.EvKey Vty.KEnter [])
        _            -> st { asShowHelp = False }

    ClickModalCancel ->
      st { asMode = ModeNormal, asShowHelp = False, asStatusMsg = "Operación cancelada." }

    _ -> st

  _ -> st

-- | Ejecuta una acción rápida invocada desde la barra de herramientas interactiva.
runQuickAction :: QuickAction -> AppState -> AppState
runQuickAction act st = case act of
  ActEditCell      -> enterEditMode (st { asFocus = FocusGrid, asShowHelp = False })
  ActAddSpecies    -> st { asMode = ModeCommand "add ", asShowHelp = False, asStatusMsg = ":add <Nombre de la especie>" }
  ActAddVariant    -> st { asMode = ModeCommand "add+ ", asShowHelp = False, asStatusMsg = ":add+ <Nombre de la variante>" }
  ActSave          -> executeCommandPure "w" (st { asMode = ModeNormal, asShowHelp = False })
  ActTreeParsimony -> computeTreeView "parsimony" (st { asMode = ModeNormal, asShowHelp = False })
  ActTreeNJ        -> computeTreeView "nj" (st { asMode = ModeNormal, asShowHelp = False })
  ActKeyEs         -> computeKeyView LangEs (st { asMode = ModeNormal, asShowHelp = False })
  ActCycleTheme    ->
    let nextTh = cycleTheme (asTheme st)
    in st
         { asTheme     = nextTh
         , asShowHelp  = False
         , asStatusMsg = "Tema cromático cambiado a " <> themeDisplayName nextTh <> " (24-bit TrueColor)."
         }
  ActToggleHelp    -> st { asShowHelp = not (asShowHelp st) }
  ActQuit          -> executeCommandPure "q" (st { asMode = ModeNormal, asShowHelp = False })

-- | Manejo de eventos en Modo Normal ($\mu = \textsc{Normal}$).
stepNormalMode :: AppState -> Vty.Event -> AppState
stepNormalMode st ev = case ev of
  -- Movimiento horizontal (h / Left, l / Right)
  Vty.EvKey (Vty.KChar 'h') [] -> moveCursor 0 (-1) st
  Vty.EvKey Vty.KLeft       [] -> moveCursor 0 (-1) st
  Vty.EvKey (Vty.KChar 'l') [] -> moveCursor 0 1 st
  Vty.EvKey Vty.KRight      [] -> moveCursor 0 1 st

  -- Movimiento vertical (k / Up, j / Down)
  Vty.EvKey (Vty.KChar 'k') [] -> moveCursor (-1) 0 st
  Vty.EvKey Vty.KUp         [] -> moveCursor (-1) 0 st
  Vty.EvKey (Vty.KChar 'j') [] -> moveCursor 1 0 st
  Vty.EvKey Vty.KDown       [] -> moveCursor 1 0 st
  Vty.EvKey Vty.KPageDown   [] -> moveCursor (max 1 (asVpRows st `div` 2)) 0 st
  Vty.EvKey Vty.KPageUp     [] -> moveCursor (-(max 1 (asVpRows st `div` 2))) 0 st
  Vty.EvKey Vty.KHome       [] -> setCursorPos (asCursorRow st) 1 st
  Vty.EvKey Vty.KEnd        [] -> setCursorPos (asCursorRow st) (numCharsState st) st

  -- Saltos rápidos estilo VIM
  Vty.EvKey (Vty.KChar 'g') [] -> setCursorPos 0 (asCursorCol st) st
  Vty.EvKey (Vty.KChar 'G') [] -> setCursorPos (max 0 (numTaxaState st - 1)) (asCursorCol st) st
  Vty.EvKey (Vty.KChar '0') [] -> setCursorPos (asCursorRow st) 1 st
  Vty.EvKey (Vty.KChar '$') [] -> setCursorPos (asCursorRow st) (numCharsState st) st
  Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl] ->
    let halfPage = max 1 (asVpRows st `div` 2)
    in moveCursor halfPage 0 st
  Vty.EvKey (Vty.KChar 'u') [Vty.MCtrl] ->
    let halfPage = max 1 (asVpRows st `div` 2)
    in moveCursor (-halfPage) 0 st

  -- Alternar vista activa con Tab o teclas directas 1..4 / F1..F4
  Vty.EvKey (Vty.KChar '\t') [] -> switchFocus (cycleFocusPanel (asFocus st)) st
  Vty.EvKey Vty.KBackTab     [] -> switchFocus (cycleFocusPanel (asFocus st)) st
  Vty.EvKey (Vty.KChar '1')  [] -> switchFocus FocusGrid st
  Vty.EvKey (Vty.KChar '2')  [] -> switchFocus FocusLinter st
  Vty.EvKey (Vty.KChar '3')  [] -> switchFocus FocusTree st
  Vty.EvKey (Vty.KChar '4')  [] -> switchFocus FocusKey st

  -- Ayuda interactiva (? o F1)
  Vty.EvKey (Vty.KChar '?') [] -> st { asShowHelp = not (asShowHelp st) }
  Vty.EvKey (Vty.KFun 1)    [] -> st { asShowHelp = not (asShowHelp st) }

  -- Asignación rápida en Modo Normal: '-' marca Inapplicable, 'U' marca Unknown
  Vty.EvKey (Vty.KChar '-') [] -> applyCellEditText "-" st
  Vty.EvKey (Vty.KChar 'u') [] -> applyCellEditText "U" st

  -- Entrar al Modo Edición (i, e o Enter)
  Vty.EvKey (Vty.KChar 'i') [] -> enterEditMode st
  Vty.EvKey (Vty.KChar 'e') [] -> enterEditMode st
  Vty.EvKey Vty.KEnter      [] -> enterEditMode st

  -- Atajo estilo VIM 'o' para agregar una nueva especie (:add <Nombre>) y 'O' para variante (:add+ <Nombre>)
  Vty.EvKey (Vty.KChar 'o') [] ->
    st { asMode = ModeCommand "add ", asStatusMsg = ":add " }
  Vty.EvKey (Vty.KChar 'O') [] ->
    st { asMode = ModeCommand "add+ ", asStatusMsg = ":add+ " }

  -- Guardado rápido con Ctrl+s
  Vty.EvKey (Vty.KChar 's') [Vty.MCtrl] ->
    executeCommandPure "w" st

  -- Entrar al Modo Comando (:)
  Vty.EvKey (Vty.KChar ':') [] ->
    st { asMode = ModeCommand "", asStatusMsg = ":" }

  -- Entrar al Modo Búsqueda (/)
  Vty.EvKey (Vty.KChar '/') [] ->
    st { asMode = ModeSearch "", asStatusMsg = "/" }

  -- Salida rápida con Ctrl+q
  Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl] ->
    st { asShouldQuit = True }

  _ -> st

-- | Entra al modo de edición para la celda bajo el cursor.
enterEditMode :: AppState -> AppState
enterEditMode st =
  let preloaded = currentCellDeltaSyntax st
      promptMsg =
        "EDITANDO (" <> T.pack (show (asCursorRow st + 1)) <> ", C"
        <> T.pack (show (asCursorCol st)) <> ") [actual: "
        <> (if T.null preloaded then "sin codificar" else preloaded) <> "]"
  in st
       { asMode      = ModeEdit ""
       , asStatusMsg = promptMsg
       }

-- | Obtiene la sintaxis DELTA actual de la celda bajo el cursor.
currentCellDeltaSyntax :: AppState -> Text
currentCellDeltaSyntax st =
  case lookupCellAttributedState (asCursorRow st) (asCursorCol st) st of
    Nothing -> ""
    Just (AttributedState raw orig) ->
      case orig of
        OriginInapplicableDependency _ -> "-"
        _ -> renderAttribute raw

-- | Manejo de eventos en Modo Edición ($\mu = \textsc{Edit}$).
stepEditMode :: AppState -> Text -> Vty.Event -> AppState
stepEditMode st buf ev = case ev of
  Vty.EvKey Vty.KEsc [] ->
    st { asMode = ModeNormal, asStatusMsg = "Edición cancelada." }

  Vty.EvKey Vty.KBS [] ->
    let buf' = if T.null buf then "" else T.init buf
    in st { asMode = ModeEdit buf' }

  Vty.EvKey (Vty.KChar ch) [] ->
    st { asMode = ModeEdit (buf <> T.singleton ch) }

  Vty.EvKey Vty.KEnter [] ->
    let effectiveBuf =
          if T.null (T.strip buf)
            then currentCellDeltaSyntax st
            else T.strip buf
    in if T.null effectiveBuf
         then st { asMode = ModeNormal, asStatusMsg = "Celda sin cambios." }
         else applyCellEditText effectiveBuf st

  _ -> st

-- | Parsea en vivo una expresión de atributo DELTA con 'pAttribute', actualiza el
-- 'ItemDef' de la fila activa, propaga el DAG de '*DEPENDENT CHARACTERS' mediante
-- 'recomputePipelineState' y refresca los diagnósticos del Linter.
applyCellEditText :: Text -> AppState -> AppState
applyCellEditText rawInput st =
  let row   = asCursorRow st
      col   = asCursorCol st
      specs = dsSpecs (asDataset st)
      mType = Map.lookup col (specCharTypes specs)
      cleanInput = T.strip rawInput
  in case runParser (sc *> pAttribute mType Nothing <* sc <* eof) "<tui-edit>" cleanInput of
       Left _parseErr ->
         st
           { asMode      = ModeNormal
           , asStatusMsg = "[ERROR SINTÁCTICO DELTA] Expresión inválida para C"
                        <> T.pack (show col) <> ": '" <> cleanInput <> "'"
           }
       Right newAttr ->
         let oldItems = dsItems (asDataset st)
             inhibitedDependents = newlyInhibitedDependents specs col newAttr
             updatedItems = updateItemAtRow row col newAttr inhibitedDependents oldItems
             newDataset = (asDataset st) { dsItems = updatedItems }
             newOverlay = Map.insert (row, col) newAttr (asOverlay st)
             stRecomputed = recomputePipelineState $ st
               { asDataset = newDataset
               , asOverlay = newOverlay
               , asMode    = ModeNormal
               , asDirty   = True
               }
             cellDiags = lookupCellDiagnostics row col stRecomputed
             statusTxt = case find (\d -> diagSeverity d == SeverityError) cellDiags of
               Just errDiag ->
                 "[" <> diagCode errDiag <> "] " <> diagMessage errDiag
               Nothing ->
                 "Celda (T" <> T.pack (show (row + 1)) <> ", C" <> T.pack (show col)
                 <> ") actualizada a '" <> renderAttribute newAttr <> "'."
         in stRecomputed { asStatusMsg = statusTxt }

-- | Determina qué caracteres dependientes quedan inhabilitados transitivamente cuando
-- el carácter controlador @col@ se edita a @newAttr@.
newlyInhibitedDependents :: SpecsDef -> Int -> RawAttribute -> Set.Set Int
newlyInhibitedDependents specs col newAttr =
  let rules = specDependencies specs
      directObs = case newAttr of
        AttrStates _ expr -> collectActiveStates expr
        _                 -> Set.empty
      directInapp = case newAttr of
        AttrStates _ (PseudoState Inapplicable _) -> True
        _                                         -> False
      stepInh inhSet =
        let added =
              [ dep
              | DependentRule ctrl disStates deps <- rules
              , let triggered =
                      (ctrl == col && (directInapp || isDependencyTriggered directObs disStates))
                      || Set.member ctrl inhSet
              , triggered
              , dep <- deps
              ]
            inhSet' = Set.union inhSet (Set.fromList added)
        in if inhSet' == inhSet then inhSet else stepInh inhSet'
  in stepInh Set.empty

-- | Actualiza los atributos del ítem en la fila @targetRow@ ($0 \dots N-1$).
updateItemAtRow
  :: Int
  -> Int
  -> RawAttribute
  -> Set.Set Int
  -> [ItemDef]
  -> [ItemDef]
updateItemAtRow targetRow col newAttr inhibitedDeps items =
  [ if idx == targetRow
      then updateSingleItem item
      else item
  | (idx, item) <- zip [0 ..] items
  ]
  where
    updateSingleItem item =
      let filtered =
            [ (c, a)
            | (c, a) <- itemAttributes item
            , c /= col
            , not (Set.member c inhibitedDeps)
            ]
          inserted = insertSorted (col, newAttr) filtered
      in item { itemAttributes = inserted }

    insertSorted (c, a) [] = [(c, a)]
    insertSorted (c, a) ((c', a') : rest)
      | c < c'    = (c, a) : (c', a') : rest
      | otherwise = (c', a') : insertSorted (c, a) rest

-- | Manejo de eventos en Modo Comando ($\mu = \textsc{Command}$).
stepCommandMode :: AppState -> Text -> Vty.Event -> AppState
stepCommandMode st cmd ev = case ev of
  Vty.EvKey Vty.KEsc [] ->
    st { asMode = ModeNormal, asStatusMsg = "Comando cancelado." }

  Vty.EvKey Vty.KBS [] ->
    if T.null cmd
      then st { asMode = ModeNormal, asStatusMsg = "" }
      else let cmd' = T.init cmd
           in st { asMode = ModeCommand cmd', asStatusMsg = ":" <> cmd' }

  Vty.EvKey (Vty.KChar ch) [] ->
    let cmd' = cmd <> T.singleton ch
    in st { asMode = ModeCommand cmd', asStatusMsg = ":" <> cmd' }

  Vty.EvKey Vty.KEnter [] ->
    executeCommandPure (T.strip cmd) (st { asMode = ModeNormal })

  _ -> st

-- | Ejecuta un comando Ex (@:w@, @:q@, @:wq@, @:q!@, @:add@, @:tree@, @:key@, @:export@) de forma pura.
executeCommandPure :: Text -> AppState -> AppState
executeCommandPure rawCmd st =
  let clean = T.dropWhile (== ':') (T.strip rawCmd)
      tokens = T.words clean
  in case tokens of
       [] -> st { asStatusMsg = "" }

       ["help"] ->
         st { asShowHelp = True }

       ["w"] ->
         st
           { asDirty     = False
           , asPendingIO = asPendingIO st ++ [ActionSaveDataset (asDatasetDir st)]
           , asStatusMsg = "Dataset canónico guardado en '" <> T.pack (asDatasetDir st) <> "'."
           }

       ["w", outDir] ->
         let dirPath = T.unpack outDir
         in st
              { asDirty     = False
              , asPendingIO = asPendingIO st ++ [ActionSaveDataset dirPath]
              , asStatusMsg = "Dataset canónico guardado en '" <> outDir <> "'."
              }

       ["q"] ->
         if asDirty st
           then st { asStatusMsg = "[AVISO] Hay cambios sin guardar (use :wq para guardar y salir, o :q! para forzar)." }
           else st { asShouldQuit = True, asStatusMsg = "Cerrando sesión TAXIDE." }

       ["q!"] ->
         st { asShouldQuit = True, asStatusMsg = "Cerrando sesión sin guardar." }

       ["wq"] ->
         st
           { asDirty      = False
           , asShouldQuit = True
           , asPendingIO  = asPendingIO st ++ [ActionSaveDataset (asDatasetDir st)]
           , asStatusMsg  = "Dataset guardado. Cerrando sesión TAXIDE."
           }

       ["tree"] ->
         computeTreeView "parsimony" st

       ["tree", methodTxt] ->
         computeTreeView (T.toLower methodTxt) st

       ["key"] ->
         computeKeyView LangEs st

       ["key", langTxt] ->
         let lang = if T.toLower langTxt == "en" then LangEn else LangEs
         in computeKeyView lang st

       ["linter"] ->
         st { asFocus = FocusLinter, asStatusMsg = "Vista activa: Inspector de Diagnósticos Linter." }

       ["grid"] ->
         st { asFocus = FocusGrid, asStatusMsg = "Vista activa: Hoja de Cálculo Matricial." }

       ["theme"] ->
         st
           { asStatusMsg =
               "Tema actual: " <> themeDisplayName (asTheme st)
               <> " (disponibles: :theme gruvbox | :theme nord | :theme dracula | :theme catppuccin)."
           }

       ["theme", themeTxt] ->
         case parseThemeName themeTxt of
           Nothing ->
             st
               { asStatusMsg =
                   "[ERROR] Tema desconocido: '" <> themeTxt
                   <> "' (use :theme gruvbox | nord | dracula | catppuccin)."
               }
           Just newTheme ->
             st
               { asTheme     = newTheme
               , asStatusMsg = "Tema cromático cambiado a " <> themeDisplayName newTheme <> " (24-bit TrueColor)."
               }

       ("add" : nameParts) ->
         addTaxonToDataset PrimaryItem (T.unwords nameParts) st

       ("taxon" : nameParts) ->
         addTaxonToDataset PrimaryItem (T.unwords nameParts) st

       ("add+" : nameParts) ->
         addTaxonToDataset VariantItem (T.unwords nameParts) st

       ("variant" : nameParts) ->
         addTaxonToDataset VariantItem (T.unwords nameParts) st

       ["export", fmtTxt, fileTxt] ->
         case parseExportTarget fmtTxt of
           Nothing ->
             st { asStatusMsg = "[ERROR] Formato de exportación desconocido: '" <> fmtTxt <> "' (use nexus|tnt|dwca|json|md|html|hdf5)." }
           Just targetCmd ->
             st
               { asPendingIO = asPendingIO st ++ [ActionExport targetCmd (T.unpack fileTxt)]
               , asStatusMsg = "Exportación '" <> fmtTxt <> "' programada hacia '" <> fileTxt <> "'."
               }

       _ ->
         st { asStatusMsg = "[ERROR] Comando desconocido: ':" <> clean <> "' (pulse '?' para ver la guía)." }

parseExportTarget :: Text -> Maybe ExportTargetCmd
parseExportTarget s = case T.toLower s of
  "nexus" -> Just CmdExportNexus
  "tnt"   -> Just CmdExportTnt
  "dwca"  -> Just CmdExportDwca
  "json"  -> Just CmdExportJson
  "md"    -> Just CmdExportMd
  "html"  -> Just CmdExportHtml
  "hdf5"  -> Just CmdExportHdf5
  _       -> Nothing

-- | Agrega un nuevo taxón primario ('#') o variante ('#+') al dataset en memoria,
-- recalcula el pipeline y posiciona el cursor en la nueva fila listo para editar.
addTaxonToDataset :: ItemType -> Text -> AppState -> AppState
addTaxonToDataset itype rawNameInput st =
  let cleanName = T.strip (T.dropWhile (\c -> c == '#' || c == '+') (T.strip rawNameInput))
  in if T.null cleanName
       then st { asStatusMsg = "[ERROR] Indique el nombre del taxón, ej. ':add Craspedacusta sinensis <Gaw & Kung, 1939>'." }
       else
         let mAuth = case T.breakOn "<" cleanName of
               (_, rest) | not (T.null rest) ->
                 let inside = T.takeWhile (/= '>') (T.drop 1 rest)
                 in if T.null inside then Nothing else Just (T.strip inside)
               _ -> Nothing
             newItem = ItemDef
               { itemType       = itype
               , itemName       = cleanName
               , itemAuthority  = mAuth
               , itemAttributes = []
               , itemComments   = []
               }
             ds0 = asDataset st
             newItems = dsItems ds0 ++ [newItem]
             oldSpecs = dsSpecs ds0
             newSpecs = oldSpecs
               { specMaxNumberItems = max (specMaxNumberItems oldSpecs) (length newItems)
               }
             newDataset = ds0 { dsSpecs = newSpecs, dsItems = newItems }
             stRecomputed = recomputePipelineState $ st
               { asDataset = newDataset
               , asFocus   = FocusGrid
               , asDirty   = True
               }
             newRowIdx = max 0 (numTaxaState stRecomputed - 1)
             stMoved = setCursorPos newRowIdx 1 stRecomputed
             kindLbl = case itype of
               PrimaryItem -> "Especie '# "
               VariantItem -> "Variante '#+ "
         in stMoved
              { asStatusMsg = kindLbl <> cleanName <> "' agregada en fila "
                           <> T.pack (show (newRowIdx + 1)) <> ". Haga clic en un estado o pulse 'i' para codificar."
              }

-- | Reconstruye el árbol filogenético o fenético en memoria y activa 'FocusTree'.
computeTreeView :: Text -> AppState -> AppState
computeTreeView method st =
  let specs = dsSpecs (asDataset st)
      taxa  = V.toList (asResolvedTaxa st)
      mat   = asMatrix st
      (methodLabel, idxTree, eval) = case method of
        "upgma" ->
          let dm = computeGowerMatrix specs mat
              t  = buildUPGMA dm
              ev = evaluateTreeParsimony SquareRootRule UseCenterVal specs taxa mat t
          in ("Fenética de Gower (UPGMA)", t, ev)
        "nj" ->
          let dm = computeGowerMatrix specs mat
              t  = buildNeighborJoining dm
              ev = evaluateTreeParsimony SquareRootRule UseCenterVal specs taxa mat t
          in ("Fenética de Gower (Neighbor-Joining)", t, ev)
        _ ->
          let sr = runParsimonySearch SearchSPR SquareRootRule UseCenterVal specs taxa mat
          in ("Máxima Parsimonia (Fitch/Sankoff + SPR)", srBestTree sr, srEvaluation sr)
      namedTree = mapTreeTaxa (cmTaxonNames mat) idxTree
      rendered = T.unlines
        [ "Método : " <> methodLabel
        , "L(T)   : " <> T.pack (show (teTotalLength eval))
          <> "   |   CI: " <> T.pack (take 6 (show (teConsistencyIndex eval)))
          <> "   |   RI: " <> T.pack (take 6 (show (teRetentionIndex eval)))
          <> "   |   RC: " <> T.pack (take 6 (show (teRescaledCI eval)))
        , "Newick : " <> renderNewick namedTree
        , ""
        , T.stripEnd (renderAsciiTree namedTree)
        ]
  in st
       { asCachedTree = Just rendered
       , asFocus      = FocusTree
       , asStatusMsg  = "Árbol reconstruido (" <> methodLabel <> ", L=" <> T.pack (show (teTotalLength eval)) <> ")."
       }

-- | Construye la clave dicotómica por entropía de Shannon en memoria y activa 'FocusKey'.
computeKeyView :: KeyLang -> AppState -> AppState
computeKeyView lang st =
  let specs   = dsSpecs (asDataset st)
      chars   = dsChars (asDataset st)
      taxa    = V.toList (asResolvedTaxa st)
      mat     = asMatrix st
      keyTree = buildIdentificationKey defaultKeyOptions specs chars taxa mat
      txt     = renderBracketedKey lang keyTree
      langLbl = case lang of
        LangEs -> "Español"
        LangEn -> "Inglés"
  in st
       { asCachedKey = Just txt
       , asFocus     = FocusKey
       , asStatusMsg  = "Clave dicotómica generada por entropía de Shannon (" <> langLbl <> ")."
       }

-- | Manejo de eventos en Modo Búsqueda ($\mu = \textsc{Search}$).
stepSearchMode :: AppState -> Text -> Vty.Event -> AppState
stepSearchMode st qry ev = case ev of
  Vty.EvKey Vty.KEsc [] ->
    st { asMode = ModeNormal, asStatusMsg = "Búsqueda cancelada." }

  Vty.EvKey Vty.KBS [] ->
    if T.null qry
      then st { asMode = ModeNormal, asStatusMsg = "" }
      else let qry' = T.init qry
           in st { asMode = ModeSearch qry', asStatusMsg = "/" <> qry' }

  Vty.EvKey (Vty.KChar ch) [] ->
    let qry' = qry <> T.singleton ch
    in st { asMode = ModeSearch qry', asStatusMsg = "/" <> qry' }

  Vty.EvKey Vty.KEnter [] ->
    executeSearch (T.strip qry) (st { asMode = ModeNormal })

  _ -> st

-- | Busca una subcadena en los nombres de taxones o características y posiciona el cursor.
executeSearch :: Text -> AppState -> AppState
executeSearch needle st
  | T.null needle = st { asStatusMsg = "" }
  | otherwise =
      let lowerNeedle = T.toLower needle
          taxaList = zip [0 ..] (V.toList (asResolvedTaxa st))
          mTaxonMatch = find (\(_, rt) -> lowerNeedle `T.isInfixOf` T.toLower (rtName rt)) taxaList
          charsList = Map.toAscList (asCharMap st)
          mCharMatch = find (\(_, cd) -> lowerNeedle `T.isInfixOf` T.toLower (charFeature cd)) charsList
      in case mTaxonMatch of
           Just (rowIdx, rt) ->
             let st' = setCursorPos rowIdx (asCursorCol st) st
             in st' { asStatusMsg = "Taxón encontrado: " <> rtName rt <> " (fila " <> T.pack (show (rowIdx + 1)) <> ")." }
           Nothing -> case mCharMatch of
             Just (cid, cd) ->
               let st' = setCursorPos (asCursorRow st) cid st
               in st' { asStatusMsg = "Carácter encontrado: #" <> T.pack (show cid) <> " " <> charFeature cd <> "." }
             Nothing ->
               st { asStatusMsg = "Sin coincidencias para '/" <> needle <> "'." }

-- | Cambia el panel enfocado asegurando que las cachés bajo demanda estén pobladas.
switchFocus :: FocusPanel -> AppState -> AppState
switchFocus newFocus st =
  case newFocus of
    FocusTree | Nothing <- asCachedTree st -> computeTreeView "parsimony" st
    FocusKey  | Nothing <- asCachedKey st  -> computeKeyView LangEs st
    _ -> st
      { asFocus     = newFocus
      , asStatusMsg = "Vista activa: " <> focusTitle newFocus
      }
  where
    focusTitle FocusGrid   = "Hoja de Cálculo Matricial (2D Viewport)"
    focusTitle FocusLinter = "Inspector de Diagnósticos Linter"
    focusTitle FocusTree   = "Visor de Árbol Filogenético"
    focusTitle FocusKey    = "Visor de Clave Dicotómica"

-- | Desplaza el cursor sumando $(\Delta r, \Delta c)$ y ajusta el viewport.
moveCursor :: Int -> Int -> AppState -> AppState
moveCursor dRow dCol st =
  setCursorPos (asCursorRow st + dRow) (asCursorCol st + dCol) st

-- | Fija el cursor dentro de los límites válidos $[0, N-1] \times [1, M]$ y preserva
-- el invariante de virtualización $(r, c) \in \mathcal{V}(r_0, c_0, H_{\text{rows}}, W_{\text{cols}})$.
setCursorPos :: Int -> Int -> AppState -> AppState
setCursorPos targetRow targetCol st =
  let nTaxa  = max 1 (numTaxaState st)
      nChars = max 1 (numCharsState st)
      clampedRow = max 0 (min (nTaxa - 1) targetRow)
      clampedCol = max 1 (min nChars targetCol)
  in adjustViewport $ st
       { asCursorRow = clampedRow
       , asCursorCol = clampedCol
       }

-- | Ajusta los desplazamientos $(r_0, c_0)$ del viewport para mantener el cursor visible.
adjustViewport :: AppState -> AppState
adjustViewport st =
  let r    = asCursorRow st
      c    = asCursorCol st
      r0   = asVpRowOffset st
      c0   = asVpColOffset st
      h    = max 1 (asVpRows st)
      w    = max 1 (asVpCols st)
      r0'  | r < r0         = r
           | r >= r0 + h    = r - h + 1
           | otherwise      = r0
      c0'  | c < c0         = c
           | c >= c0 + w    = c - w + 1
           | otherwise      = c0
  in st
       { asVpRowOffset = max 0 r0'
       , asVpColOffset = max 1 c0'
       }

-- | Manejador de eventos de Brick que delega teclado y redimensionado a 'stepTUIEvent',
-- clics y rueda de ratón a 'stepTUIMouse', y ejecuta efectos de guardado/exportación en 'IO'.
handleAppEvent :: BrickEvent ResourceName e -> EventM ResourceName AppState ()
handleAppEvent (VtyEvent vtyEv) = do
  st <- get
  let st' = stepTUIEvent st vtyEv
  stFinal <- liftIO (flushPendingIOActions st')
  put stFinal
  when (asShouldQuit stFinal) halt
handleAppEvent (MouseDown rn btn mods _loc) = do
  st <- get
  let st' = stepTUIMouse st rn btn mods
  stFinal <- liftIO (flushPendingIOActions st')
  put stFinal
  when (asShouldQuit stFinal) halt
handleAppEvent _ = pure ()

-- | Ejecuta y vacía la cola de acciones de E/S pendientes ('asPendingIO').
flushPendingIOActions :: AppState -> IO AppState
flushPendingIOActions st
  | null (asPendingIO st) = pure st
  | otherwise = do
      let actions = asPendingIO st
          stCleared = st { asPendingIO = [] }
      foldlMExec stCleared actions
  where
    foldlMExec cur [] = pure cur
    foldlMExec cur (act : rest) = do
      cur' <- runSingleAction cur act
      foldlMExec cur' rest

    runSingleAction cur (ActionSaveDataset outDir) = do
      writeDeltaDataset outDir (asDataset cur)
      pure cur

    runSingleAction cur (ActionExport targetCmd outPath) = do
      let ds   = asDataset cur
          meta :: DatasetMetadata
          meta = emptyDatasetMetadata "TAXIDE Exported Dataset"
          ctx  = mkExportContext meta (dsSpecs ds) (dsChars ds) (V.toList (asResolvedTaxa cur)) (asMatrix cur)
      case targetCmd of
        CmdExportNexus ->
          case renderNexus (NexusOptions SquareRootRule UseCenterVal) ctx of
            Left err  -> pure cur { asStatusMsg = renderExportError err }
            Right txt -> TIO.writeFile outPath txt >> pure cur
        CmdExportTnt ->
          case renderTnt (TntOptions SquareRootRule UseCenterVal) ctx of
            Left err  -> pure cur { asStatusMsg = renderExportError err }
            Right txt -> TIO.writeFile outPath txt >> pure cur
        CmdExportMd -> do
          let txt = renderNaturalMarkdown defaultNaturalOptions ctx
          TIO.writeFile outPath txt
          pure cur
        CmdExportHtml -> do
          let txt = renderNaturalHtml defaultNaturalOptions ctx
          TIO.writeFile outPath txt
          pure cur
        CmdExportDwca -> do
          let bytes = renderDwcArchive (DwcOptions defaultNaturalOptions) ctx
          BL.writeFile outPath bytes
          pure cur
        CmdExportJson -> do
          let bytes = renderJson ctx
          BL.writeFile outPath bytes
          pure cur
        CmdExportHdf5 -> do
          let bytes = renderHpcBinary ctx
          BL.writeFile outPath bytes
          pure cur
