{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Taxide.Core.Diagnostic
Description : Sistema de diagnósticos y alertas del linter taxonómico en tiempo real
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Core.Diagnostic
  ( Severity (..)
  , SourcePos (..)
  , Diagnostic (..)
  , formatDiagnostic
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Severidad del mensaje de diagnóstico.
data Severity = Info | Warning | Error
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Posición en el archivo fuente DELTA.
data SourcePos = SourcePos
  { posFile :: !FilePath
  , posLine :: !Int
  , posCol  :: !Int
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Registro estructurado de un diagnóstico de validación sintáctica o biológica.
data Diagnostic = Diagnostic
  { diagPos      :: !(Maybe SourcePos) -- ^ Posición en el código fuente si está disponible
  , diagSeverity :: !Severity           -- ^ Nivel de severidad
  , diagCode     :: !Text               -- ^ Código del linter (ej. "TIDE-DEP-001")
  , diagMessage  :: !Text               -- ^ Mensaje explicativo en lenguaje humano
  } deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

-- | Formatea un diagnóstico en texto legible para consola o logs.
formatDiagnostic :: Diagnostic -> Text
formatDiagnostic d =
  let posStr = case diagPos d of
        Just p  -> T.pack (posFile p) <> ":" <> T.pack (show (posLine p)) <> ":" <> T.pack (show (posCol p)) <> ": "
        Nothing -> ""
      sevStr = case diagSeverity d of
        Info    -> "[INFO]"
        Warning -> "[AVISO]"
        Error   -> "[ERROR]"
  in posStr <> sevStr <> " (" <> diagCode d <> ") " <> diagMessage d
