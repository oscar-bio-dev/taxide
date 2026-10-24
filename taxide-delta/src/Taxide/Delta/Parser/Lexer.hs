{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Delta.Parser.Lexer
Description : Analizador léxico resiliente para la sintaxis del estándar DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Delta.Parser.Lexer
  ( Parser
  , sc
  , scn
  , lexeme
  , symbol
  , pInteger
  , pDouble
  , pNestedComment
  , pDirectivePrefix
  , pDefinitionTerminator
  , isTerminatorFollowed
  ) where

import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Tipo de parser base Megaparsec sobre 'Text'.
type Parser = Parsec Void Text

-- | Consumidor de espacios dentro de la misma línea o a través de líneas.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Consumidor de espacios que preserva o maneja fines de línea explícitos.
scn :: Parser ()
scn = L.space (void (takeWhile1P (Just "inline space") (\c -> c == ' ' || c == '\t'))) empty empty

-- | Envoltorio para tokens léxicos con consumo posterior de espacios en blanco.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parser para un símbolo literal con consumo posterior de espacios.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parser de enteros con signo o sin signo.
pInteger :: Integral a => Parser a
pInteger = lexeme L.decimal

-- | Parser de números reales / coma flotante.
pDouble :: Parser Double
pDouble = try (lexeme L.float) <|> (fromIntegral <$> (pInteger :: Parser Integer))

-- | Parser recursivo para comentarios anidados delimitados por '<' y '>'.
-- Maneja anidamiento arbitrario: <hojas <raramente lobuladas> pubescentes>.
pNestedComment :: Parser Text
pNestedComment = between (char '<') (char '>') pInner
  where
    pInner = do
      parts <- many (pNested <|> pNonBracketText)
      pure (T.concat parts)
    pNested = do
      inner <- pNestedComment
      pure ("<" <> inner <> ">")
    pNonBracketText = takeWhile1P (Just "comment content") (\c -> c /= '<' && c /= '>')

-- | Parser de directiva DELTA comenzando por '*' y una secuencia de palabras clave
-- donde solo las primeras 3 letras de cada palabra son significativas.
-- Ejemplo: "NUMBER OF CHARACTERS" coincide con "*NUMBER OF CHARACTERS", "*NUM OF CHA", "*NUMBERS OF CHARS".
pDirectivePrefix :: [Text] -> Parser ()
pDirectivePrefix wordsList = lexeme $ do
  _ <- char '*'
  sc
  mapM_ parseWord wordsList
  where
    parseWord expected = do
      let expLower = T.toLower expected
          len = T.length expLower
          prefixLen = if len >= 3 then 3 else len
          prefix = T.take prefixLen expLower
      _ <- string' prefix
      _ <- takeWhileP (Just "rest of word") isAlphaNum
      sc

-- | Desambiguador contextual del terminador de definición '/'.
-- En DELTA, el símbolo '/' actúa como terminador de definición de un carácter o ítem
-- si y solo si le sigue un espacio en blanco, fin de línea o fin de archivo (EOF).
-- No captura '/' cuando se usa como operador lógico OR en atributos (ej. "1/2")
-- o como texto literal en palabras (ej. "and/or").
pDefinitionTerminator :: Parser ()
pDefinitionTerminator = lexeme $ do
  _ <- char '/'
  lookAhead isTerminatorFollowed

-- | Comprueba si el carácter siguiente a '/' cumple la regla de terminador.
isTerminatorFollowed :: Parser ()
isTerminatorFollowed =
  void (satisfy (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')) <|> eof
