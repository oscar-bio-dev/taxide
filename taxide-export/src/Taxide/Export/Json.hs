{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Export.Json
Description : Serializador JSON tipado (GeoJSON-compatible) listo para ingesta en PostGIS
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Emite un @FeatureCollection@ GeoJSON cuyas @Feature@ llevan @geometry: null@ y
todos los atributos resueltos en @properties@. Cada @Feature@ puede ingerirse
con @ogr2ogr@ o @ST_GeomFromGeoJSON@ y @jsonb_to_record@ en PostgreSQL/PostGIS, y
la geometría puede completarse después con datos de distribución. Las claves
del objeto se emiten ordenadas, por lo que la salida es determinista.
-}

module Taxide.Export.Json
  ( renderJson
  , jsonValue
  ) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Taxide.Core.Character (CharacterDef (..))
import Taxide.Core.Item (ItemType (..))
import Taxide.Core.Types
  ( DatasetMetadata (..)
  , PseudoValue (..)
  , RawAttribute (..)
  , RawStateExpr (..)
  , StatRange (..)
  )
import Taxide.Engine.Discretize (quantitativeRange)
import Taxide.Engine.Variants (AttrOrigin (..), AttributedState (..), ResolvedTaxon (..))
import Taxide.Export.Common
import Taxide.Export.DarwinCore (DwcTaxon (..), dwcTaxa)

-- | Serializa la matriz resuelta como documento JSON compacto.
renderJson :: ExportContext -> BL.ByteString
renderJson = encode . jsonValue

-- | Valor JSON de la matriz resuelta.
jsonValue :: ExportContext -> Value
jsonValue ctx = object
  [ "type" .= ("FeatureCollection" :: Text)
  , "taxide" .= object
      [ "format"     .= ("taxide-json" :: Text)
      , "version"    .= ("0.1" :: Text)
      , "dataset"    .= datasetValue (ecMetadata ctx)
      , "characters" .= map charValue (ecChars ctx)
      ]
  , "features" .= zipWith feature (dwcTaxa ctx) (ecTaxa ctx)
  ]
  where
    feature d t = object
      [ "type"       .= ("Feature" :: Text)
      , "id"         .= dtId d
      , "geometry"   .= Null
      , "properties" .= object
          [ "index"     .= rtIndex t
          , "name"      .= rtName t
          , "authority" .= rtAuthority t
          , "rank"      .= nullIfEmpty (dtRank d)
          , "item_type" .= (case rtType t of
                              PrimaryItem -> "primary"
                              VariantItem -> "variant" :: Text)
          , "parent_id" .= nullIfEmpty (dtParentId d)
          , "attributes" .= [ attributeValue ctx cid as | (cid, as) <- Map.toAscList (rtAttributes t) ]
          ]
      ]

nullIfEmpty :: Text -> Maybe Text
nullIfEmpty t = if T.null t then Nothing else Just t

datasetValue :: DatasetMetadata -> Value
datasetValue m = object
  [ "title"      .= datasetTitle m
  , "creators"   .= datasetCreators m
  , "abstract"   .= datasetAbstract m
  , "license"    .= datasetLicense m
  , "contact"    .= datasetContact m
  , "citation"   .= datasetCitation m
  , "identifier" .= datasetIdentifier m
  ]

charValue :: CharacterDef -> Value
charValue c = object
  [ "id"      .= charNumber c
  , "type"    .= show (charType c)
  , "feature" .= charFeature c
  , "heading" .= charHeading c
  , "units"   .= charUnits c
  , "states"  .= [ object ["id" .= s, "label" .= l] | (s, l) <- charStates c ]
  ]

attributeValue :: ExportContext -> Int -> AttributedState -> Value
attributeValue ctx cid (AttributedState raw origin) = object $
  [ "character" .= cid
  , "feature"   .= ctxFeature ctx cid
  , "type"      .= show (ctxCharType ctx cid)
  , "origin"    .= originText origin
  ] ++ body
  where
    body = case raw of
      AttrStates mc expr -> statesBody mc expr
      AttrInteger mc _   -> numericBody "integer" mc Nothing
      AttrReal mc _ u    -> numericBody "real" mc u
      AttrText mc txt    ->
        [ "kind" .= ("text" :: Text), "status" .= ("valid" :: Text), "text" .= txt, "comment" .= mc ]

    statesBody mc expr = case expr of
      PseudoState p _ ->
        [ "kind" .= ("states" :: Text)
        , "status" .= (case p of
                         Unknown      -> "unknown"
                         Inapplicable -> "inapplicable"
                         Variable     -> "variable" :: Text)
        , "comment" .= mc
        ]
      _ ->
        let states = exprStates expr
        in [ "kind"     .= ("states" :: Text)
           , "status"   .= ("valid" :: Text)
           , "operator" .= (case exprKind expr of
                              VkSingle      -> "single"
                              VkPolymorphic -> "and"
                              VkUncertain   -> "or" :: Text)
           , "states"   .= states
           , "labels"   .= [ctxStateLabel ctx cid s | s <- states]
           , "comment"  .= mc
           ]

    numericBody kind mc mUnit = case quantitativeRange raw of
      Nothing -> []
      Just sr ->
        [ "kind"          .= (kind :: Text)
        , "status"        .= ("valid" :: Text)
        , "absolute_min"  .= srAbsoluteMin sr
        , "normal_min"    .= srNormalMin sr
        , "center"        .= srCenterVal sr
        , "normal_max"    .= srNormalMax sr
        , "absolute_max"  .= srAbsoluteMax sr
        , "unit"          .= firstJust (mUnit : [ctxUnits ctx cid])
        , "comment"       .= mc
        ]

    firstJust xs = case concatMap maybeToList xs of
      (x : _) | not (T.null (T.strip x)) -> Just (T.strip x)
      _ -> Nothing

    originText o = case o of
      OriginExplicit                -> "explicit"
      OriginInherited p             -> "inherited:" <> p
      OriginImplicitOmitted _       -> "implicit_omitted"
      OriginImplicitUnspecified _   -> "implicit_unspecified"
      OriginInapplicableDependency c -> "inapplicable_dependency:" <> T.pack (show c)
