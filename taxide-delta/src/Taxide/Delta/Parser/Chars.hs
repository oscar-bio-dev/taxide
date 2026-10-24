{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Delta.Parser.Chars
Description : Parser Megaparsec para el archivo de definiciones de caracteres (CHARS)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Delta.Parser.Chars
  ( pCharsFile
  , parseCharsText
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

import Taxide.Core.Character (CharacterDef (..))
import Taxide.Core.Types (CharacterType (..), StateId)
import Taxide.Delta.Parser.Lexer

-- | Parsea el archivo CHARS completo utilizando el mapa opcional de tipos de caracteres.
pCharsFile :: Map Int CharacterType -> Parser [CharacterDef]
pCharsFile typeMap = do
  sc
  skipDirectivesBeforeHash
  chars <- many (pSingleCharacter typeMap)
  eof
  pure chars
  where
    skipDirectivesBeforeHash =
      many (try (char '*' *> takeWhileP Nothing (\c -> c /= '*' && c /= '#') *> sc)) *> pure ()

-- | Parsea una definición individual de carácter: #<num>. <feature>/ <states>...
pSingleCharacter :: Map Int CharacterType -> Parser CharacterDef
pSingleCharacter typeMap = do
  _ <- char '#'
  sc
  charNum <- pInteger
  _ <- char '.'
  sc

  -- Posible comentario de encabezado: <features of leaves>
  mHeading <- optional (try (lexeme pNestedComment))

  -- Nombre de la característica biológica hasta el primer '/' o '<'
  (feature, mUnits) <- pFeatureAndUnits

  -- Consumir terminador '/' del encabezado/característica
  pDefinitionTerminator

  -- Determinar tipo de carácter declarado o inferido
  let declaredType = Map.lookup charNum typeMap

  -- Si es multiestado, parsear los estados correspondientes
  states <- case declaredType of
    Just TE -> pure []
    Just IN -> pure []
    Just RN -> pure []
    _       -> many (try pStateDef)

  let finalType = case declaredType of
        Just t  -> t
        Nothing -> if not (null states)
                     then UM
                     else case mUnits of
                            Just _  -> RN
                            Nothing -> TE

  pure $ CharacterDef
    { charNumber   = charNum
    , charType     = finalType
    , charHeading  = mHeading
    , charFeature  = T.strip feature
    , charUnits    = mUnits
    , charStates   = states
    , charComments = []
    }

-- | Parsea el texto del carácter y extrae posibles unidades entre corchetes angulares.
pFeatureAndUnits :: Parser (Text, Maybe Text)
pFeatureAndUnits = do
  parts <- many (choice
    [ try (Left <$> pNestedComment)
    , Right <$> takeWhile1P (Just "feature text") (\c -> c /= '/' && c /= '<')
    ])
  let (comments, texts) = partitionParts parts
      rawFeature = T.concat texts
      -- Si hay un comentario al final o con unidades tipo <cm>, lo extraemos
      mUnits = case comments of
        (u:_) -> Just u
        []    -> Nothing
  pure (rawFeature, mUnits)
  where
    partitionParts [] = ([], [])
    partitionParts (Left c : rest)  = let (cs, ts) = partitionParts rest in (c : cs, ts)
    partitionParts (Right t : rest) = let (cs, ts) = partitionParts rest in (cs, t : ts)

-- | Parsea la definición de un estado discreto: "1. simple/"
pStateDef :: Parser (StateId, Text)
pStateDef = lexeme $ do
  stNum <- (pInteger :: Parser StateId)
  _ <- char '.'
  sc
  descParts <- many (choice
    [ try ((\c -> "<" <> c <> ">") <$> pNestedComment)
    , takeWhile1P (Just "state description") (\c -> c /= '/' && c /= '<')
    ])
  pDefinitionTerminator
  pure (stNum, T.strip (T.concat descParts))

-- | Función pura para parsear una cadena de texto CHARS.
parseCharsText :: Map Int CharacterType -> FilePath -> Text -> Either Text [CharacterDef]
parseCharsText typeMap path input =
  case runParser (pCharsFile typeMap) path input of
    Left err  -> Left (T.pack (errorBundlePretty err))
    Right res -> Right res
