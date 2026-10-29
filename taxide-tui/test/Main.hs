{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Suite de pruebas headless para taxide-tui (MVU, viewport 2D, DAG reactivo y comandos)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Graphics.Vty as Vty
import Test.Tasty
import Test.Tasty.HUnit

import Taxide.Core.Character (CharacterDef, mkMultistateChar)
import Taxide.Core.Item (ItemDef, mkPrimaryItem)
import Taxide.Core.Specs (DependentRule (..), SpecsDef (..), emptySpecs)
import Taxide.Core.Types
  ( CharacterType (..)
  , PseudoValue (..)
  , RawAttribute (..)
  , RawStateExpr (..)
  , StateToken (..)
  )
import Taxide.Delta.Pipeline (DeltaDataset (..))
import Taxide.Engine.Diagnostic (LintDiagnostic (..))
import Taxide.Engine.Matrix (ColumnarMatrix (..), tagInapplicable, tagValid)
import Taxide.Engine.Variants (AttrOrigin (..), AttributedState (..))
import Taxide.TUI.Draw (cellAttrAndText)
import Taxide.TUI.Runner
  ( applyTerminalResize
  , initAppState
  , stepTUIEvent
  , stepTUIEvents
  , stepTUIMouse
  )
import Taxide.TUI.State
  ( AppMode (..)
  , AppState (..)
  , FocusPanel (..)
  , PendingIOAction (..)
  , QuickAction (..)
  , ResourceName (..)
  , lookupCellAttributedState
  , responsiveCharColWidth
  , responsiveTaxonColWidth
  , visibleViewportBounds
  )
import Taxide.TUI.Theme
  ( ThemeName (..)
  , attrCellError
  , attrCellImplicit
  , attrCellInapplicable
  , attrCellValid
  , themePalette
  , tpBg0
  , gruvboxBg0
  , nordBg0
  , draculaBg0
  , catppuccinBg0
  )

-- | Helpers para construir eventos sintéticos de teclado Vty sin TTY físico.
keyChar :: Char -> Vty.Event
keyChar c = Vty.EvKey (Vty.KChar c) []

keyEnter :: Vty.Event
keyEnter = Vty.EvKey Vty.KEnter []

keyEsc :: Vty.Event
keyEsc = Vty.EvKey Vty.KEsc []

typeTextEvents :: Text -> [Vty.Event]
typeTextEvents = map keyChar . T.unpack

-- | Dataset sintético de prueba con 3 taxones, 3 caracteres y la regla de dependencia 1,2:2
-- (cuando el carácter 1 toma el estado 2 "hojas ausentes", el carácter 2 "margen foliar"
-- pasa a ser biológicamente inaplicable '-').
fixtureDataset :: DeltaDataset
fixtureDataset =
  let specs = emptySpecs
        { specNumCharacters   = 3
        , specMaxNumberStates = 3
        , specMaxNumberItems  = 3
        , specCharTypes       = Map.fromList [(1, UM), (2, UM), (3, OM)]
        , specNumbersOfStates = Map.fromList [(1, 2), (2, 2), (3, 3)]
        , specDependencies    =
            [ DependentRule
                { depControllerChar  = 1
                , depDisablingStates = Set.singleton 2
                , depDependentChars  = [2]
                }
            ]
        }
      chars :: [CharacterDef]
      chars =
        [ mkMultistateChar 1 UM "hojas" [(1, "presentes"), (2, "ausentes")]
        , mkMultistateChar 2 UM "margen foliar" [(1, "entero"), (2, "aserrado")]
        , mkMultistateChar 3 OM "porte" [(1, "herbáceo"), (2, "arbustivo"), (3, "arbóreo")]
        ]
      stAttr s = AttrStates Nothing (SingleState (StateToken s Nothing))
      items :: [ItemDef]
      items =
        [ mkPrimaryItem "Quercus robur"     [(1, stAttr 1), (2, stAttr 1), (3, stAttr 3)]
        , mkPrimaryItem "Pinus sylvestris"  [(1, stAttr 1), (2, stAttr 2), (3, stAttr 3)]
        , mkPrimaryItem "Ephedra distachya" [(1, stAttr 2), (3, stAttr 2)]
        ]
  in DeltaDataset
       { dsSpecs = specs
       , dsChars = chars
       , dsItems = items
       }

main :: IO ()
main = defaultMain $ testGroup "TAXIDE TUI Headless Verification Suite (Fase 5)"
  [ testCase "1. Navegación 2D (hjkl, g/G, 0/$) y límites del Viewport [0, N-1] x [1, M]" $ do
      let st0 = (initAppState "." fixtureDataset)
                  { asVpRows = 2
                  , asVpCols = 2
                  }
      asCursorRow st0 @?= 0
      asCursorCol st0 @?= 1
      visibleViewportBounds st0 @?= ((0, 1), (1, 2))

      -- Intentar moverse fuera del borde superior/izquierdo (h, k en (0,1)) mantiene (0,1)
      let stClamped0 = stepTUIEvents st0 [keyChar 'h', keyChar 'k']
      asCursorRow stClamped0 @?= 0
      asCursorCol stClamped0 @?= 1

      -- Desplazarse con j, j, l, l hasta (2, 3) desplaza el viewport automáticamente
      let stMoved = stepTUIEvents st0 [keyChar 'j', keyChar 'j', keyChar 'l', keyChar 'l']
      asCursorRow stMoved @?= 2
      asCursorCol stMoved @?= 3
      asVpRowOffset stMoved @?= 1
      asVpColOffset stMoved @?= 2
      visibleViewportBounds stMoved @?= ((1, 2), (2, 3))

      -- Intentar exceder (N-1, M) con j, l adicionales mantiene el cursor en (2, 3)
      let stClampedMax = stepTUIEvents stMoved [keyChar 'j', keyChar 'l']
      asCursorRow stClampedMax @?= 2
      asCursorCol stClampedMax @?= 3

      -- Saltos rápidos g y 0 regresan a (0, 1); G y $ van a (2, 3)
      let stHome = stepTUIEvents stClampedMax [keyChar 'g', keyChar '0']
      asCursorRow stHome @?= 0
      asCursorCol stHome @?= 1

      let stEnd = stepTUIEvents stHome [keyChar 'G', keyChar '$']
      asCursorRow stEnd @?= 2
      asCursorCol stEnd @?= 3

  , testCase "2. Propagación reactiva del DAG: editar controlador (0,1) a estado 2 inhabilita (0,2) a '-'" $ do
      let st0 = initAppState "." fixtureDataset
      -- Inicialmente (0, 1) y (0, 2) son válidos en Quercus robur
      fst (cellAttrAndText 0 1 st0) @?= attrCellValid
      cellAttrAndText 0 2 st0 @?= (attrCellValid, "1")

      -- Inyectar secuencia: 'i' -> '2' -> Enter sobre la celda (0, 1)
      let editCtrlEvents = [keyChar 'i', keyChar '2', keyEnter]
          stAfterCtrl = stepTUIEvents st0 editCtrlEvents

      asMode stAfterCtrl @?= ModeNormal
      asDirty stAfterCtrl @?= True

      -- Verificar que la celda dependiente (0, 2) pasó automáticamente a Inapplicable ('-')
      case lookupCellAttributedState 0 2 stAfterCtrl of
        Nothing -> assertFailure "Se esperaba un atributo resuelto en (0, 2)"
        Just (AttributedState raw orig) -> do
          raw @?= AttrStates Nothing (PseudoState Inapplicable Nothing)
          orig @?= OriginInapplicableDependency 1

      -- Verificar en la matriz columnar SoA que la etiqueta de (0, 2) es tagInapplicable
      let tagsCol2 = cmDiscreteTags (asMatrix stAfterCtrl) Map.! 2
      (tagsCol2 VU.! 0) @?= tagInapplicable
      cellAttrAndText 0 2 stAfterCtrl @?= (attrCellInapplicable, "-")

  , testCase "3. Linter en vivo: editar celda dependiente inhabilitada (0,2) con estado activo dispara TAX-DEP-002" $ do
      let st0 = initAppState "." fixtureDataset
          -- Paso 1: inhabilitar (0, 2) editando el controlador (0, 1) al estado 2
          stInh = stepTUIEvents st0 [keyChar 'i', keyChar '2', keyEnter]
      -- Confirmar que aún no hay error TAX-DEP-002 en Quercus robur
      any (\d -> diagCode d == "TAX-DEP-002") (asDiagnostics stInh) @?= False

      -- Paso 2: mover el cursor a la celda dependiente (0, 2) con 'l' e introducir estado activo '1'
      let stViolated = stepTUIEvents stInh [keyChar 'l', keyChar 'i', keyChar '1', keyEnter]
      asCursorRow stViolated @?= 0
      asCursorCol stViolated @?= 2

      -- El panel del Linter en vivo debe registrar inmediatamente TAX-DEP-002
      let depErrors = [d | d <- asDiagnostics stViolated, diagCode d == "TAX-DEP-002"]
      assertBool "Se esperaba diagnóstico TAX-DEP-002 en vivo" (not (null depErrors))

      -- Y la celda (0, 2) debe colorearse con attrCellError (#fb4934) manteniendo '-' en la matriz resuelta
      fst (cellAttrAndText 0 2 stViolated) @?= attrCellError
      let tagsCol2 = cmDiscreteTags (asMatrix stViolated) Map.! 2
      (tagsCol2 VU.! 0) @?= tagInapplicable

  , testCase "4. Comandos interactivos ':tree parsimony', ':tree nj' y ':key es' actualizan las vistas" $ do
      let st0 = initAppState "." fixtureDataset
          runCmd cmdTxt st =
            stepTUIEvents st ([keyChar ':'] ++ typeTextEvents cmdTxt ++ [keyEnter])

          stTree = runCmd "tree parsimony" st0
      asFocus stTree @?= FocusTree
      case asCachedTree stTree of
        Nothing -> assertFailure "Se esperaba árbol filogenético en caché tras :tree parsimony"
        Just treeTxt -> do
          assertBool "El árbol debe incluir Quercus robur" ("Quercus robur" `T.isInfixOf` treeTxt)
          assertBool "El árbol debe incluir representación Newick (;)" (";" `T.isInfixOf` treeTxt)

      let stTreeNJ = runCmd "tree nj" stTree
      asFocus stTreeNJ @?= FocusTree
      case asCachedTree stTreeNJ of
        Nothing -> assertFailure "Se esperaba fenograma NJ en caché tras :tree nj"
        Just njTxt ->
          assertBool "El informe debe mencionar Neighbor-Joining" ("Neighbor-Joining" `T.isInfixOf` njTxt)

      let stKey = runCmd "key es" stTreeNJ
      asFocus stKey @?= FocusKey
      case asCachedKey stKey of
        Nothing -> assertFailure "Se esperaba clave dicotómica en caché tras :key es"
        Just keyTxt -> do
          assertBool "La clave debe contener Ephedra distachya" ("Ephedra distachya" `T.isInfixOf` keyTxt)
          assertBool "La clave debe contener Quercus robur" ("Quercus robur" `T.isInfixOf` keyTxt)

  , testCase "5. Búsqueda incremental ('/'), alternancia de vistas (Tab) y comandos :w / :q" $ do
      let st0 = initAppState "." fixtureDataset
          -- Buscar '/Ephedra' debe saltar a la fila 2
          stSearched = stepTUIEvents st0 ([keyChar '/'] ++ typeTextEvents "Ephedra" ++ [keyEnter])
      asCursorRow stSearched @?= 2

      -- Buscar '/porte' debe saltar a la columna 3
      let stCharSearch = stepTUIEvents stSearched ([keyChar '/'] ++ typeTextEvents "porte" ++ [keyEnter])
      asCursorCol stCharSearch @?= 3

      -- Alternar vistas con Tab (Grid -> Linter -> Tree -> Key -> Grid)
      let stTab1 = stepTUIEvent st0 (keyChar '\t')
          stTab2 = stepTUIEvent stTab1 (keyChar '\t')
          stTab3 = stepTUIEvent stTab2 (keyChar '\t')
          stTab4 = stepTUIEvent stTab3 (keyChar '\t')
      asFocus stTab1 @?= FocusLinter
      asFocus stTab2 @?= FocusTree
      asFocus stTab3 @?= FocusKey
      asFocus stTab4 @?= FocusGrid

      -- Cancelar edición con Esc conserva la celda intacta
      let stCancel = stepTUIEvents st0 [keyChar 'i', keyChar '2', keyEsc]
      asMode stCancel @?= ModeNormal
      asDirty stCancel @?= False
      let tagsCol1 = cmDiscreteTags (asMatrix stCancel) Map.! 1
      (tagsCol1 VU.! 0) @?= tagValid

      -- Comando :add agrega una nueva especie, posiciona el cursor en la última fila y permite editarla
      let stAdded = stepTUIEvents st0 ([keyChar ':'] ++ typeTextEvents "add Abies alba <Mill.>" ++ [keyEnter])
      asCursorRow stAdded @?= 3
      asCursorCol stAdded @?= 1
      asDirty stAdded @?= True

      -- Comando :wq encola ActionSaveDataset y activa asShouldQuit
      let stWQ = stepTUIEvents st0 ([keyChar ':'] ++ typeTextEvents "wq" ++ [keyEnter])
      asShouldQuit stWQ @?= True
      asPendingIO stWQ @?= [ActionSaveDataset "."]

  , testCase "6. Diseño responsivo al maximizar terminal (190x50) e interacción directa con ratón" $ do
      let st0 = initAppState "." fixtureDataset
          stMax = applyTerminalResize 190 50 st0
      asTermWidth stMax @?= 190
      asTermHeight stMax @?= 50
      assertBool "Columna taxonómica se expande en pantalla ancha" (responsiveTaxonColWidth stMax >= 34)
      assertBool "Columnas de caracteres se expanden para llenar el ancho disponible" (responsiveCharColWidth stMax >= 20)
      assertBool "Filas del viewport crecen al maximizar altura" (asVpRows stMax > 25)

      -- Click izquierdo en celda (1, 2) selecciona la celda; segundo click abre ModeEdit
      let stClick1 = stepTUIMouse stMax (ClickCell 1 2) Vty.BLeft []
      asCursorRow stClick1 @?= 1
      asCursorCol stClick1 @?= 2
      asMode stClick1 @?= ModeNormal

      let stClick2 = stepTUIMouse stClick1 (ClickCell 1 2) Vty.BLeft []
      asMode stClick2 @?= ModeEdit ""

      -- Click en botón Cancelar del modal cierra ModeEdit
      let stCancelModal = stepTUIMouse stClick2 ClickModalCancel Vty.BLeft []
      asMode stCancelModal @?= ModeNormal

      -- Click en píldora de estado [1:entero] asigna el estado en 1 solo click y marca asDirty
      let stPill = stepTUIMouse stCancelModal (ClickSetState "1") Vty.BLeft []
      asDirty stPill @?= True
      cellAttrAndText 1 2 stPill @?= (attrCellValid, "1")

      -- Click en pestaña Árbol calcula el cladograma y cambia el foco a FocusTree
      let stTabClick = stepTUIMouse stPill (ClickTab FocusTree) Vty.BLeft []
      asFocus stTabClick @?= FocusTree
      assertBool "Se genera árbol automáticamente al hacer click en pestaña [3:Árbol]" (asCachedTree stTabClick /= Nothing)

      -- Click en acción rápida [? Ayuda] muestra y oculta el modal de ayuda
      let stHelpOn  = stepTUIMouse stTabClick (ClickQuickAction ActToggleHelp) Vty.BLeft []
          stHelpOff = stepTUIMouse stHelpOn (ClickQuickAction ActToggleHelp) Vty.BLeft []
      asShowHelp stHelpOn @?= True
      asShowHelp stHelpOff @?= False

  , testCase "7. Cambio dinámico de temas (:theme gruvbox | nord | dracula | catppuccin)" $ do
      let st0 = initAppState "." fixtureDataset
          runCmd cmdTxt st =
            stepTUIEvents st ([keyChar ':'] ++ typeTextEvents cmdTxt ++ [keyEnter])

      -- Por defecto inicia en Gruvbox Dark
      asTheme st0 @?= ThemeGruvbox
      tpBg0 (themePalette (asTheme st0)) @?= gruvboxBg0

      -- :theme nord
      let stNord = runCmd "theme nord" st0
      asTheme stNord @?= ThemeNord
      tpBg0 (themePalette (asTheme stNord)) @?= nordBg0

      -- :theme dracula
      let stDracula = runCmd "theme dracula" stNord
      asTheme stDracula @?= ThemeDracula
      tpBg0 (themePalette (asTheme stDracula)) @?= draculaBg0

      -- :theme catppuccin
      let stCatppuccin = runCmd "theme catppuccin" stDracula
      asTheme stCatppuccin @?= ThemeCatppuccin
      tpBg0 (themePalette (asTheme stCatppuccin)) @?= catppuccinBg0

      -- :theme gruvbox regresa a Gruvbox
      let stGruvbox = runCmd "theme gruvbox" stCatppuccin
      asTheme stGruvbox @?= ThemeGruvbox

      -- Click en botón [Tema: Gruvbox] alterna al siguiente tema (Nord)
      let stCycled = stepTUIMouse stGruvbox (ClickQuickAction ActCycleTheme) Vty.BLeft []
      asTheme stCycled @?= ThemeNord

  , testCase "8. Restauración de atributo heredado/implícito con 'x' y 'Del' (clearCellAttribute)" $ do
      let st0 = initAppState "." fixtureDataset
          -- Agregar variante '#+ Ephedra var. minor' al final (fila 3), que hereda de Ephedra distachya (fila 2)
          stVar = stepTUIEvents st0 ([keyChar ':'] ++ typeTextEvents "add+ Ephedra var. minor" ++ [keyEnter])
          -- Mover cursor a columna 3 (porte) de la variante (fila 3, columna 3)
          stAtVarC3 = stepTUIEvents stVar [keyChar 'l', keyChar 'l']

      asCursorRow stAtVarC3 @?= 3
      asCursorCol stAtVarC3 @?= 3
      -- Inicialmente (3, 3) es heredado de Ephedra distachya con estado "2"
      cellAttrAndText 3 3 stAtVarC3 @?= (attrCellImplicit, "2")
      fmap asOrigin (lookupCellAttributedState 3 3 stAtVarC3) @?= Just (OriginInherited "Ephedra distachya")

      -- Sobrescribir explícitamente (3, 3) con estado '1'
      let stOverridden = stepTUIEvents stAtVarC3 [keyChar 'i', keyChar '1', keyEnter]
      cellAttrAndText 3 3 stOverridden @?= (attrCellValid, "1")
      fmap asOrigin (lookupCellAttributedState 3 3 stOverridden) @?= Just OriginExplicit

      -- Pulsar 'x' elimina la sobrescritura explícita y restaura el estado heredado "2"
      let stRestored = stepTUIEvent stOverridden (keyChar 'x')
      cellAttrAndText 3 3 stRestored @?= (attrCellImplicit, "2")
      fmap asOrigin (lookupCellAttributedState 3 3 stRestored) @?= Just (OriginInherited "Ephedra distachya")

      -- Mover a (3, 2), introducir '1' (disparando TAX-DEP-002) y restaurar con tecla Del
      let stViolated = stepTUIEvents stRestored [keyChar 'h', keyChar 'i', keyChar '1', keyEnter]
      fst (cellAttrAndText 3 2 stViolated) @?= attrCellError
      let stClearedDel = stepTUIEvent stViolated (Vty.EvKey Vty.KDel [])
      cellAttrAndText 3 2 stClearedDel @?= (attrCellInapplicable, "-")
  ]

