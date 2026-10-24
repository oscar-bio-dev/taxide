{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Delta.Parser.Items
Description : Parser Megaparsec para el archivo de descripciones de taxones (ITEMS)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Delta.Parser.Items
  ( pItemsFile
  , parseItemsText
  , pAttribute
  , pStateExpr
  , pStatRange
  ) where

import Control.Monad (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Taxide.Core.Item (ItemDef (..), ItemType (..))
import Taxide.Core.Types
import Taxide.Delta.Parser.Lexer

-- | Parsea el archivo ITEMS completo.
pItemsFile :: Map Int CharacterType -> Parser [ItemDef]
pItemsFile typeMap = do
  sc
  skipDirectivesBeforeHash
  items <- many (pSingleItem typeMap)
  eof
  pure items
  where
    skipDirectivesBeforeHash =
      many (try (char '*' *> takeWhileP Nothing (\c -> c /= '*' && c /= '#') *> sc)) *> pure ()

-- | Parsea un único ítem taxonómico (primario '#' o variante '#+').
pSingleItem :: Map Int CharacterType -> Parser ItemDef
pSingleItem typeMap = do
  _ <- char '#'
  isVariant <- (char '+' *> pure True) <|> pure False
  sc
  -- Nombre del taxón hasta el terminador '/'
  parsedParts <- many (choice
    [ Left <$> try pNestedComment
    , Right <$> takeWhile1P (Just "taxon name") (\c -> c /= '/' && c /= '<')
    ])
  pDefinitionTerminator

  -- Atributos morfológicos hasta el siguiente '#' o fin de archivo
  attrs <- manyTill (lexeme (pCharAttribute typeMap)) (lookAhead (void (char '#') <|> eof))

  let reconstruct (Left c)  = "<" <> c <> ">"
      reconstruct (Right t) = t
      rawName = T.strip (T.concat (map reconstruct parsedParts))
      mAuth = case [c | Left c <- parsedParts] of
        (c:_) -> Just (T.strip c)
        []    -> Nothing

  pure $ ItemDef
    { itemType       = if isVariant then VariantItem else PrimaryItem
    , itemName       = rawName
    , itemAuthority  = mAuth
    , itemAttributes = attrs
    , itemComments   = []
    }

-- | Parsea un atributo individual precedido por su número de carácter: "c,attr".
pCharAttribute :: Map Int CharacterType -> Parser (Int, RawAttribute)
pCharAttribute typeMap = do
  charNum <- pInteger
  _ <- char ','
  -- Comentario de carácter opcional prefijado: c<comentario>,...
  mPrefixComment <- optional (try pNestedComment)
  sc
  let mType = Map.lookup charNum typeMap
  attr <- pAttribute mType mPrefixComment
  pure (charNum, attr)

-- | Parsea el valor del atributo según el tipo de carácter biológico.
pAttribute :: Maybe CharacterType -> Maybe Text -> Parser RawAttribute
pAttribute mType mPrefixComment = case mType of
  Just TE -> do
    textVal <- takeWhile1P (Just "text attribute") (\c -> c /= '/' && c /= '#')
    _ <- optional (char '/')
    pure (AttrText mPrefixComment (T.strip textVal))

  Just IN -> do
    sr <- pStatRange (fromIntegral <$> (L.signed sc (lexeme L.decimal) :: Parser Integer))
    pure (AttrInteger mPrefixComment sr)

  Just RN -> do
    sr <- pStatRange pDouble
    mUnits <- optional (try pNestedComment)
    pure (AttrReal mPrefixComment sr mUnits)

  Just _multistate -> do
    expr <- pStateExpr
    pure (AttrStates mPrefixComment expr)

  Nothing -> do
    -- Detección heurística si no se suministró el mapa de tipos
    choice
      [ try (AttrStates mPrefixComment <$> pStateExpr)
      , try (do sr <- pStatRange pDouble
                mUnits <- optional (try pNestedComment)
                pure (AttrReal mPrefixComment sr mUnits))
      , do textVal <- takeWhile1P (Just "fallback text") (\c -> c /= '/' && c /= '#')
           _ <- optional (char '/')
           pure (AttrText mPrefixComment (T.strip textVal))
      ]

-- | Parsea una expresión sintáctica de estados combinando OR (/), AND (&) y rangos (-).
pStateExpr :: Parser RawStateExpr
pStateExpr = pOrExpr
  where
    pOrExpr = do
      firstAnd <- pAndExpr
      restAnds <- many (char '/' *> pAndExpr)
      pure (foldl StateOr firstAnd restAnds)

    pAndExpr = do
      firstTerm <- pRangeOrSingle
      restTerms <- many (char '&' *> pRangeOrSingle)
      pure (foldl StateAnd firstTerm restTerms)

    pRangeOrSingle = choice
      [ try pRangeTo
      , try pSingleStateToken
      , pPseudoStateExpr
      ]

    pRangeTo = do
      t1 <- pToken
      _ <- char '-'
      t2 <- pToken
      pure (RangeTo t1 t2)

    pSingleStateToken = SingleState <$> pToken

    pToken = do
      stNum <- (pInteger :: Parser StateId)
      mCom <- optional (try pNestedComment)
      pure (StateToken stNum mCom)

    pPseudoStateExpr = do
      pseudo <- choice
        [ char 'U' *> pure Unknown
        , char '-' *> pure Inapplicable
        , char 'V' *> pure Variable
        ]
      mCom <- optional (try pNestedComment)
      pure (PseudoState pseudo mCom)

-- | Parsea rangos cuantitativos y estadística descriptiva de DELTA:
-- Formas admitidas:
--   * @(min-)n1-med-n2(-max)@
--   * @(min-)n1-n2@
--   * @n1-n2(-max)@
--   * @n1-med-n2@
--   * @n@
pStatRange :: Parser a -> Parser (StatRange a)
pStatRange pNum = do
  mAbsMin <- optional (try pExtremeMin)
  n1 <- pNum
  mAfterN1 <- optional (char '-' *> pNum)
  case mAfterN1 of
    Nothing -> do
      mAbsMax <- optional (try pExtremeMax)
      pure $ StatRange mAbsMin n1 Nothing n1 mAbsMax
    Just n2 -> do
      mAfterN2 <- optional (char '-' *> pNum)
      case mAfterN2 of
        Nothing -> do
          mAbsMax <- optional (try pExtremeMax)
          pure $ StatRange mAbsMin n1 Nothing n2 mAbsMax
        Just n3 -> do
          -- 3 valores normales: n1 es min, n2 es med, n3 es max
          mAbsMax <- optional (try pExtremeMax)
          pure $ StatRange mAbsMin n1 (Just n2) n3 mAbsMax
  where
    pExtremeMin = between (char '(') (char ')') (pNum <* char '-')
    pExtremeMax = between (char '(') (char ')') (char '-' *> pNum)

-- | Función pura para parsear una cadena de texto ITEMS.
parseItemsText :: Map Int CharacterType -> FilePath -> Text -> Either Text [ItemDef]
parseItemsText typeMap path input =
  case runParser (pItemsFile typeMap) path input of
    Left err  -> Left (T.pack (errorBundlePretty err))
    Right res -> Right res
