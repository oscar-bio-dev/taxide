{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Delta.Serializer.Chars
Description : Serializador canónico para definiciones de caracteres (CHARS) de DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Delta.Serializer.Chars
  ( renderChars
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Taxide.Core.Character (CharacterDef (..))

-- | Renderiza una lista de 'CharacterDef' a texto canónico DELTA formateado.
renderChars :: [CharacterDef] -> Text
renderChars chars = T.unlines $
  [ "*SHOW TAXIDE Canonical CHARS"
  , ""
  , "*CHARACTER DESCRIPTIONS"
  , ""
  ] ++ concatMap renderSingleChar chars

renderSingleChar :: CharacterDef -> [Text]
renderSingleChar c =
  let numStr = T.pack (show (charNumber c))
      headingStr = case charHeading c of
        Just h  -> "<" <> h <> "> "
        Nothing -> ""
      unitsStr = case charUnits c of
        Just u  -> "<" <> u <> ">"
        Nothing -> ""
      headerLine = "#" <> numStr <> ". " <> headingStr <> charFeature c <> unitsStr <> "/"
      stateLines = map renderState (charStates c)
  in [headerLine] ++ stateLines ++ [""]
  where
    renderState (stId, desc) =
      "   " <> T.pack (show stId) <> ". " <> desc <> "/"
