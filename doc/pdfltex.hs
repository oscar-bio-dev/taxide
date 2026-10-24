{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Main
Description : Compilador inteligente y resiliente de documentos LaTeX a PDF.
Copyright   : (c) TAXIDE Scientific Computing Team, 2026
License     : Apache-2.0

Este script detecta automáticamente el mejor backend disponible en el sistema
(pdflatex nativo o contenedor TeX Live en Docker), ejecuta múltiples pasadas
para resolver referencias cruzadas/TOC, extrae diagnósticos precisos en caso de
error y reporta métricas del PDF generado.
-}

module Main (main) where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (when)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory
  ( canonicalizePath
  , doesFileExist
  , findExecutable
  , getFileSize
  , removeFile
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath
  ( addExtension
  , hasExtension
  , replaceExtension
  , splitFileName
  , takeBaseName
  , takeDirectory
  , takeExtension
  , takeFileName
  , (</>)
  )
import System.IO
  ( IOMode (ReadMode)
  , hClose
  , hGetContents
  , hSetEncoding
  , latin1
  , openFile
  )
import System.Posix.User (getRealGroupID, getRealUserID)
import System.Process
  ( CreateProcess (cwd, std_err, std_in, std_out)
  , StdStream (CreatePipe)
  , createProcess
  , proc
  , waitForProcess
  )

-- | Backend de ejecución para la compilación LaTeX.
data Backend
  = LocalPdfLatex !FilePath
  | DockerPdfLatex !FilePath !String -- (Ruta a binario docker, nombre de imagen)
  deriving (Show, Eq)

-- | Opciones de invocación CLI.
data CompileConfig = CompileConfig
  { cfgTargetFile :: !FilePath
  , cfgCleanAux   :: !Bool
  , cfgPasses     :: !Int
  } deriving (Show, Eq)

-- | Detecta el backend disponible: prioridad nativo, fallback a contenedor Docker.
detectBackend :: IO (Either String Backend)
detectBackend = do
  mNative <- findExecutable "pdflatex"
  case mNative of
    Just nativePath -> pure $ Right (LocalPdfLatex nativePath)
    Nothing -> do
      mDocker <- findExecutable "docker"
      case mDocker of
        Just dockerPath -> do
          -- Imagen TeXLive estándar verificada en el entorno
          pure $ Right (DockerPdfLatex dockerPath "texlive/texlive:latest-medium")
        Nothing ->
          pure $ Left
            "No se encontró 'pdflatex' en el PATH ni un runtime de 'docker' para ejecución aislada."

-- | Analiza los argumentos de la línea de comandos.
parseArgs :: [String] -> Either String CompileConfig
parseArgs [] = Left "Debes proporcionar la ruta al archivo .tex."
parseArgs args =
  let cleanFlag = "--clean" `elem` args
      nonFlags  = filter (/= "--clean") args
  in case nonFlags of
       [file] -> Right CompileConfig
         { cfgTargetFile = file
         , cfgCleanAux   = cleanFlag
         , cfgPasses     = 2
         }
       _ -> Left "Número de argumentos inválido."

-- | Ejecuta un proceso capturando stdout y stderr con codificación resiliente (latin1).
safeReadProcess :: CreateProcess -> IO (ExitCode, String, String)
safeReadProcess cp = do
  let cp' = cp { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  (Just hin, Just hout, Just herr, ph) <- createProcess cp'
  hClose hin
  hSetEncoding hout latin1
  hSetEncoding herr latin1
  out <- hGetContents hout
  err <- hGetContents herr
  _ <- evaluate (length out)
  _ <- evaluate (length err)
  exitCode <- waitForProcess ph
  pure (exitCode, out, err)

-- | Ejecuta una pasada de compilación con pdflatex (nativo o vía Docker).
runLatexPass :: Backend -> FilePath -> String -> Int -> Int -> IO (Either String ())
runLatexPass backend workDir baseFileName currentPass totalPasses = do
  putStrLn $ "  [" ++ show currentPass ++ "/" ++ show totalPasses ++ "] Compilando: " ++ baseFileName ++ "..."
  
  uid <- getRealUserID
  gid <- getRealGroupID
  let userFlag = show uid ++ ":" ++ show gid
      baseArgs =
        [ "-interaction=nonstopmode"
        , "-halt-on-error"
        , "-file-line-error"
        , baseFileName
        ]
      (cmd, args) = case backend of
        LocalPdfLatex bin ->
          (bin, baseArgs)
        DockerPdfLatex dockerBin img ->
          ( dockerBin
          , [ "run"
            , "--rm"
            , "--user"
            , userFlag
            , "-v"
            , workDir ++ ":/work:z"
            , "-w"
            , "/work"
            , img
            , "pdflatex"
            ] ++ baseArgs
          )

  let cp = (proc cmd args) { cwd = Just workDir }
  (exitCode, stdoutStr, _stderrStr) <- safeReadProcess cp
  case exitCode of
    ExitSuccess -> pure (Right ())
    ExitFailure code -> do
      -- Extraer diagnósticos del log o stdout para informar el fallo exacto
      let logPath = workDir </> replaceExtension baseFileName "log"
      logExists <- doesFileExist logPath
      errReport <- if logExists
        then extractErrorFromLog logPath
        else pure (unlines (takeLastLines 20 (lines stdoutStr)))
      pure $ Left $
        "Fallo en la pasada " ++ show currentPass ++ " (Código " ++ show code ++ "):\n" ++ errReport

-- | Extrae líneas de error ('!') de un archivo .log de TeX.
extractErrorFromLog :: FilePath -> IO String
extractErrorFromLog path = do
  content <- (do
    h <- openFile path ReadMode
    hSetEncoding h latin1
    c <- hGetContents h
    _ <- evaluate (length c)
    hClose h
    pure c) `catch` \(_ :: SomeException) -> pure ""
  let ls = lines content
      errors = filter (\l -> "!" `isPrefixOf` l || "Error:" `isPrefixOf` l) ls
  if null errors
    then pure (unlines (takeLastLines 15 ls))
    else pure (unlines (take 10 errors))

-- | Obtiene las últimas N líneas de una lista.
takeLastLines :: Int -> [a] -> [a]
takeLastLines n ls = drop (max 0 (length ls - n)) ls

-- | Limpia archivos auxiliares generados por LaTeX.
cleanAuxiliaryFiles :: FilePath -> String -> IO ()
cleanAuxiliaryFiles dir base = do
  let exts = [".aux", ".log", ".out", ".toc", ".synctex.gz"]
  putStrLn "  Limpiando archivos auxiliares (.aux, .log, .out, .toc)..."
  mapM_ (\ext -> do
          let f = dir </> replaceExtension base ext
          exists <- doesFileExist f
          when exists (removeFile f `catch` \(_ :: SomeException) -> pure ())
        ) exts

-- | Orquestador principal de la compilación.
compileDocument :: CompileConfig -> IO ()
compileDocument cfg = do
  -- Normalización de la ruta del archivo
  let rawPath = cfgTargetFile cfg
      texCandidate = if hasExtension rawPath
                       then rawPath
                       else addExtension rawPath "tex"
  
  rawExists <- doesFileExist texCandidate
  when (not rawExists) $ do
    putStrLn $ "Error: El archivo '" ++ texCandidate ++ "' no existe."
    exitFailure

  absPath <- canonicalizePath texCandidate
  let workDir  = takeDirectory absPath
      fileName = takeFileName absPath
      pdfName  = replaceExtension fileName "pdf"
      pdfPath  = workDir </> pdfName

  putStrLn "\n======================================================="
  putStrLn "          TAXIDE LaTeX Document Compiler               "
  putStrLn "======================================================="
  putStrLn $ "Documento : " ++ fileName
  putStrLn $ "Directorio: " ++ workDir

  -- Detección de Backend
  backendRes <- detectBackend
  backend <- case backendRes of
    Left errMsg -> do
      putStrLn $ "\nError de Entorno: " ++ errMsg
      putStrLn "Sugerencia: Inicia el servicio Docker o instala TeX Live (pdflatex)."
      exitFailure
    Right b -> do
      case b of
        LocalPdfLatex bin ->
          putStrLn $ "Backend   : pdflatex nativo (" ++ bin ++ ")"
        DockerPdfLatex _ img ->
          putStrLn $ "Backend   : Contenedor TeX Live (" ++ img ++ ")"
      pure b

  putStrLn "-------------------------------------------------------"

  -- Pasadas de compilación
  let runPasses current total
        | current > total = pure (Right ())
        | otherwise = do
            res <- runLatexPass backend workDir fileName current total
            case res of
              Left err -> pure (Left err)
              Right () -> runPasses (current + 1) total

  result <- runPasses 1 (cfgPasses cfg)
  case result of
    Left err -> do
      putStrLn "\n[ERROR DE COMPILACIÓN]"
      putStrLn err
      exitFailure
    Right () -> do
      pdfExists <- doesFileExist pdfPath
      if pdfExists
        then do
          bytes <- getFileSize pdfPath
          let kb = fromIntegral bytes / (1024 :: Double)
          putStrLn "\n======================================================="
          putStrLn " [ÉXITO] ¡PDF generado satisfactoriamente!"
          putStrLn $ " Archivo : " ++ pdfPath
          putStrLn $ " Tamaño  : " ++ show (round kb :: Integer) ++ " KB (" ++ show bytes ++ " bytes)"
          putStrLn "======================================================="
          when (cfgCleanAux cfg) (cleanAuxiliaryFiles workDir fileName)
          exitSuccess
        else do
          putStrLn "\n[ERROR]: El proceso terminó pero no se encontró el archivo PDF resultante."
          exitFailure

-- | Punto de entrada.
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Right cfg -> compileDocument cfg
    Left err  -> do
      putStrLn $ "Error: " ++ err
      putStrLn "Uso: runhaskell pdfltex.hs <archivo.tex> [--clean]"
      putStrLn "Ejemplo: runhaskell pdfltex.hs taxide.tex"
      exitFailure