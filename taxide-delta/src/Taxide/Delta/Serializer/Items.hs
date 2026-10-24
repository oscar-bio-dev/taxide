{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Delta.Serializer.Items
Description : Serializador canónico para descripciones de taxones (ITEMS) de DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Delta.Serializer.Items
  ( renderItems
  , renderAttribute
  , renderStateExpr
  , renderStatRange
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Taxide.Core.Item (ItemDef (..), ItemType (..))
import Taxide.Core.Types

-- | Renderiza una lista de 'ItemDef' a texto canónico DELTA formateado.
renderItems :: [ItemDef] -> Text
renderItems items = T.unlines $
  [ "*SHOW TAXIDE Canonical ITEMS"
  , ""
  , "*ITEM DESCRIPTIONS"
  , ""
  ] ++ concatMap renderSingleItem items

renderSingleItem :: ItemDef -> [Text]
renderSingleItem item =
  let prefix = case itemType item of
        PrimaryItem -> "# "
        VariantItem -> "#+ "
      headerLine = prefix <> itemName item <> "/"
      attrTokens = map renderPair (itemAttributes item)
      wrappedAttrs = wrapLines 78 "   " attrTokens
  in [headerLine] ++ wrappedAttrs ++ [""]
  where
    renderPair (c, attr) = T.pack (show c) <> "," <> renderAttribute attr

-- | Envuelve una lista de tokens en líneas de longitud máxima.
wrapLines :: Int -> Text -> [Text] -> [Text]
wrapLines _ _ [] = []
wrapLines maxLen indent (t:ts) = go [t] (T.length t) ts
  where
    go currentLine _ [] = [indent <> T.unwords (reverse currentLine)]
    go currentLine currentLen (x:xs)
      | currentLen + 1 + T.length x > maxLen =
          (indent <> T.unwords (reverse currentLine)) : go [x] (T.length x) xs
      | otherwise =
          go (x : currentLine) (currentLen + 1 + T.length x) xs

-- | Renderiza un 'RawAttribute'.
renderAttribute :: RawAttribute -> Text
renderAttribute attr = case attr of
  AttrStates mPrefix expr ->
    renderPrefix mPrefix <> renderStateExpr expr
  AttrInteger mPrefix sr ->
    renderPrefix mPrefix <> renderStatRange (T.pack . show) sr
  AttrReal mPrefix sr mUnits ->
    renderPrefix mPrefix <> renderStatRange renderDouble sr <> renderUnits mUnits
  AttrText mPrefix txt ->
    renderPrefix mPrefix <> txt <> "/"
  where
    renderPrefix Nothing  = ""
    renderPrefix (Just p) = "<" <> p <> ">"

    renderUnits Nothing   = ""
    renderUnits (Just u)  = "<" <> u <> ">"

    renderDouble d
      | d == fromInteger (round d) = T.pack (show (round d :: Integer))
      | otherwise                  = T.pack (show d)

-- | Renderiza un 'RawStateExpr'.
renderStateExpr :: RawStateExpr -> Text
renderStateExpr expr = case expr of
  SingleState tok ->
    renderStateToken tok
  RangeTo t1 t2 ->
    renderStateToken t1 <> "-" <> renderStateToken t2
  StateOr e1 e2 ->
    renderStateExpr e1 <> "/" <> renderStateExpr e2
  StateAnd e1 e2 ->
    renderStateExpr e1 <> "&" <> renderStateExpr e2
  PseudoState p mCom ->
    renderPseudo p <> renderComment mCom
  where
    renderStateToken (StateToken sid mCom) =
      T.pack (show sid) <> renderComment mCom

    renderPseudo Unknown      = "U"
    renderPseudo Inapplicable = "-"
    renderPseudo Variable     = "V"

    renderComment Nothing  = ""
    renderComment (Just c) = "<" <> c <> ">"

-- | Renderiza un rango estadístico 'StatRange a'.
renderStatRange :: Eq a => (a -> Text) -> StatRange a -> Text
renderStatRange fmt sr =
  let absMinStr = case srAbsoluteMin sr of
        Just aMin -> "(" <> fmt aMin <> "-)"
        Nothing   -> ""
      absMaxStr = case srAbsoluteMax sr of
        Just aMax -> "(-" <> fmt aMax <> ")"
        Nothing   -> ""
      normalStr = case srCenterVal sr of
        Just c  -> fmt (srNormalMin sr) <> "-" <> fmt c <> "-" <> fmt (srNormalMax sr)
        Nothing ->
          if srNormalMin sr == srNormalMax sr
            then fmt (srNormalMin sr)
            else fmt (srNormalMin sr) <> "-" <> fmt (srNormalMax sr)
  in absMinStr <> normalStr <> absMaxStr
