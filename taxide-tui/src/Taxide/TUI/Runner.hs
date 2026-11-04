{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.TUI.Runner
Description : Punto de entrada interactivo Brick y arnés de simulación headless
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Orquesta la inicialización de la sesión interactiva en terminal ('runTaxideTUI',
'runTaxideTUIFiles') sobre 'Brick.Main.defaultMain' y reexporta las funciones
puras de transición ('initAppState', 'stepTUIEvent', 'stepTUIEvents') para
ejecutar pruebas automatizadas sin requerir un dispositivo TTY físico.
-}

module Taxide.TUI.Runner
  ( -- * Definición de la Aplicación Brick
    taxideBrickApp
  , runTaxideTUI
  , runTaxideTUIFiles
  , scratchDataset

    -- * Reexportaciones para Pruebas Headless
  , initAppState
  , stepTUIEvent
  , stepTUIEvents
  , stepTUIMouse
  , applyTerminalResize
  , clearCellAttribute
  ) where

import Brick (App (..), defaultMain, getVtyHandle, modify, neverShowCursor)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Graphics.Vty as Vty
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

import Taxide.Core.Character (mkMultistateChar, mkNumericChar)
import Taxide.Core.Item (mkPrimaryItem)
import Taxide.Core.Specs (SpecsDef (..), emptySpecs)
import Taxide.Core.Types (CharacterType (..))
import Taxide.Delta.Pipeline (DeltaDataset (..), readDeltaFiles)
import Taxide.TUI.Draw (drawTUI)
import Taxide.TUI.State (AppState (..), ResourceName, initAppState)
import Taxide.TUI.Theme (themeAttrMap)
import Taxide.TUI.Update
  ( applyTerminalResize
  , clearCellAttribute
  , handleAppEvent
  , stepTUIEvent
  , stepTUIEvents
  , stepTUIMouse
  )

-- | Especificación declarativa de la aplicación Model-View-Update en Brick.
-- Habilita automáticamente soporte de ratón Vty ('Vty.Mouse'), detecta las
-- dimensiones reales de la terminal al iniciar para ocupar el 100% de la ventana
-- y proyecta dinámicamente el tema cromático activo ('asTheme').
taxideBrickApp :: App AppState e ResourceName
taxideBrickApp = App
  { appDraw         = drawTUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleAppEvent
  , appStartEvent   = do
      vty <- getVtyHandle
      let out = Vty.outputIface vty
      liftIO $ when (Vty.supportsMode out Vty.Mouse) $
        Vty.setMode out Vty.Mouse True
      (w, h) <- liftIO (Vty.displayBounds out)
      modify (applyTerminalResize w h)
  , appAttrMap      = themeAttrMap . asTheme
  }

-- | Inicia la interfaz interactiva de terminal cargando @SPECS@, @CHARS@ e @ITEMS@
-- desde el directorio indicado.
runTaxideTUI :: FilePath -> IO ()
runTaxideTUI dir =
  runTaxideTUIFiles (dir </> "SPECS") (dir </> "CHARS") (dir </> "ITEMS")

-- | Dataset en blanco inicial para sesiones interactivas sin archivos previos.
scratchDataset :: DeltaDataset
scratchDataset = DeltaDataset
  { dsSpecs = emptySpecs
      { specNumCharacters   = 3
      , specMaxNumberStates = 2
      , specMaxNumberItems  = 10
      , specCharTypes       = Map.fromList [(1, UM), (2, UM), (3, RN)]
      , specNumbersOfStates = Map.fromList [(1, 2), (2, 2)]
      }
  , dsChars =
      [ mkMultistateChar 1 UM "habitus" [(1, "arborescente"), (2, "herbáceo")]
      , mkMultistateChar 2 UM "hojas" [(1, "simples"), (2, "compuestas")]
      , mkNumericChar 3 RN "altura" (Just "m")
      ]
  , dsItems =
      [ mkPrimaryItem "Especie nueva 1" []
      ]
  }

-- | Inicia la interfaz interactiva de terminal a partir de rutas explícitas a
-- los tres archivos cardinales DELTA. Si no existen archivos en el directorio
-- especificado (por ejemplo, al ejecutar 'taxide' en una carpeta limpia sin
-- dataset previo), inicializa un espacio de trabajo en blanco en memoria.
runTaxideTUIFiles :: FilePath -> FilePath -> FilePath -> IO ()
runTaxideTUIFiles specsPath charsPath itemsPath = do
  sExists <- doesFileExist specsPath
  cExists <- doesFileExist charsPath
  iExists <- doesFileExist itemsPath
  let baseDir = takeDirectory specsPath
      dirForSave = if null baseDir || baseDir == "." then "." else baseDir

  if not (sExists || cExists || iExists)
    then do
      let st0 = (initAppState dirForSave scratchDataset)
            { asStatusMsg = "Nuevo espacio de trabajo en blanco — Use ':add <taxón>' para añadir especies, 'i' para editar, ':w' para guardar en disco."
            }
      void (defaultMain taxideBrickApp st0)
    else do
      dsRes <- readDeltaFiles specsPath charsPath itemsPath
      case dsRes of
        Left err -> do
          hPutStrLn stderr "[ERROR] Fallo al cargar el dataset DELTA para la TUI:"
          TIO.hPutStrLn stderr err
          exitFailure
        Right ds -> do
          let st0 = initAppState dirForSave ds
          void (defaultMain taxideBrickApp st0)
