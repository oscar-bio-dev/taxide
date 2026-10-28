{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Export.DarwinCore
Description : Proyección Φ_DwC-A hacia Darwin Core Archive determinista en memoria
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Implementa la proyección de la Sección 6 de @doc/taxide.tex@: construye el
núcleo @taxon.txt@, las extensiones @measurementorfact.txt@ y @description.txt@
y los descriptores @meta.xml@ y @eml.xml@, y los empaqueta en un @.zip@
reproducible byte a byte (orden de entradas y marca de tiempo fijos) sin tocar
el disco.
-}

module Taxide.Export.DarwinCore
  ( DwcOptions (..)
  , defaultDwcOptions
  , DwcTaxon (..)
  , dwcTaxa
  , dwcTaxonTxt
  , dwcMeasurementTxt
  , dwcDescriptionTxt
  , dwcMetaXml
  , dwcEmlXml
  , dwcFiles
  , renderDwcArchive
  , dwcEpoch
  ) where

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BL
import Data.List (find, mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Taxide.Core.Item (ItemType (..))
import Taxide.Core.Types
  ( DatasetMetadata (..)
  , PseudoValue (..)
  , RawAttribute (..)
  , RawStateExpr (..)
  )
import Taxide.Engine.Discretize (quantitativeRange)
import Taxide.Engine.Variants (AttrOrigin (..), AttributedState (..), ResolvedTaxon (..))
import Taxide.Export.Common
import Taxide.Export.Natural
  ( Lang (..)
  , NaturalOptions (..)
  , describeTaxa
  , defaultNaturalOptions
  , plainDescription
  )

-- | Opciones de la proyección Darwin Core.
newtype DwcOptions = DwcOptions
  { dwcNatural :: NaturalOptions  -- ^ Opciones de la prosa de @description.txt@
  } deriving (Eq, Show)

-- | Prosa en inglés con valores implícitos incluidos.
defaultDwcOptions :: DwcOptions
defaultDwcOptions = DwcOptions defaultNaturalOptions

-- | Fila del núcleo taxonómico.
data DwcTaxon = DwcTaxon
  { dtIndex     :: !Int
  , dtId        :: !Text   -- ^ @dwc:taxonID@
  , dtName      :: !Text   -- ^ @dwc:scientificName@
  , dtAuthority :: !Text   -- ^ @dwc:scientificNameAuthorship@
  , dtRank      :: !Text   -- ^ @dwc:taxonRank@ (sólo con marcador explícito)
  , dtParentId  :: !Text   -- ^ @dwc:parentNameUsageID@: π(i) de un ítem @#+@
  , dtRemarks   :: !Text   -- ^ @dwc:taxonRemarks@
  } deriving (Eq, Show)

-- | Marca de tiempo fija de las entradas del ZIP: 1980-01-01T00:00:00 (mínimo MS-DOS).
dwcEpoch :: Integer
dwcEpoch = 315532800

--------------------------------------------------------------------------------
-- Núcleo taxonómico
--------------------------------------------------------------------------------

-- | Filas del núcleo con claves únicas y estables derivadas del nombre.
dwcTaxa :: ExportContext -> [DwcTaxon]
dwcTaxa ctx = snd (mapAccumL step (Map.empty, Nothing) (ecTaxa ctx))
  where
    step (seen, lastPrimary) t =
      let base = slugify (rtName t)
          n = Map.findWithDefault (0 :: Int) base seen
          ident = if n == 0 then base else base <> "-" <> T.pack (show (n + 1))
          seen' = Map.insertWith (+) base 1 seen
          rank = rankOf (rtName t)
          (parentId, remarks) = case rtType t of
            PrimaryItem -> ("", "")
            VariantItem -> case lastPrimary of
              Nothing ->
                ("", "DELTA variant item (#+) without a preceding primary item.")
              Just (pid, pname)
                | T.null rank ->
                    (pid, "DELTA variant item (#+) of '" <> pname
                          <> "'; its nature (sex, morph, form, etc.) is not specified.")
                | otherwise ->
                    (pid, "DELTA variant item (#+) of '" <> pname <> "'.")
          lastPrimary' = case rtType t of
            PrimaryItem -> Just (ident, rtName t)
            VariantItem -> lastPrimary
          row = DwcTaxon
            { dtIndex     = rtIndex t
            , dtId        = ident
            , dtName      = rtName t
            , dtAuthority = maybe "" id (rtAuthority t)
            , dtRank      = rank
            , dtParentId  = parentId
            , dtRemarks   = remarks
            }
      in ((seen', lastPrimary'), row)

-- | Rango infraespecífico, sólo si el nombre contiene un marcador explícito.
rankOf :: Text -> Text
rankOf name = case mapMaybe marker (T.words name) of
  (r : _) -> r
  []      -> ""
  where
    marker tok = lookup (T.toLower tok)
      [ ("subsp.", "subspecies"), ("ssp.", "subspecies")
      , ("var.", "variety"), ("subvar.", "subvariety")
      , ("f.", "form"), ("forma", "form"), ("subf.", "subform")
      ]

--------------------------------------------------------------------------------
-- Tablas TSV
--------------------------------------------------------------------------------

tsv :: [Text] -> [[Text]] -> Text
tsv header rows = T.unlines (map (T.intercalate "\t" . map tsvField) (header : rows))

-- | @taxon.txt@: núcleo @dwc:Taxon@.
dwcTaxonTxt :: ExportContext -> Text
dwcTaxonTxt ctx = tsv
  ["taxonID", "scientificName", "scientificNameAuthorship", "taxonRank", "parentNameUsageID", "taxonRemarks"]
  [ [dtId d, dtName d, dtAuthority d, dtRank d, dtParentId d, dtRemarks d] | d <- dwcTaxa ctx ]

-- | @measurementorfact.txt@: una fila por celda informativa (excluye @U@ y @-@).
dwcMeasurementTxt :: ExportContext -> Text
dwcMeasurementTxt ctx = tsv
  ["coreid", "measurementType", "measurementValue", "measurementUnit", "measurementRemarks"]
  (concat (zipWith rowsFor (dwcTaxa ctx) (ecTaxa ctx)))
  where
    rowsFor d t =
      [ [dtId d, ctxFeature ctx cid, value, unit, T.intercalate "; " remarks]
      | (cid, as) <- Map.toAscList (rtAttributes t)
      , Just (value, unit, remarks) <- [measurement cid as]
      ]

    measurement cid (AttributedState raw origin) = case raw of
      AttrStates mc expr -> case expr of
        PseudoState Unknown _      -> Nothing
        PseudoState Inapplicable _ -> Nothing
        PseudoState Variable _     -> Just ("variable", "", originRemark origin ++ commentRemark mc)
        _ ->
          let labels = [ctxStateLabel ctx cid s | s <- exprStates expr]
              kindRemark = case exprKind expr of
                VkSingle      -> []
                VkPolymorphic -> ["polymorphism (simultaneous states, &)"]
                VkUncertain   -> ["uncertainty (alternative states, /)"]
          in Just (T.intercalate " | " labels, "", kindRemark ++ exprComments expr ++ commentRemark mc ++ originRemark origin)
      AttrInteger mc _ -> numeric cid raw Nothing mc origin
      AttrReal mc _ u  -> numeric cid raw u mc origin
      AttrText _ _     -> Nothing

    numeric cid raw mUnit mc origin = do
      sr <- quantitativeRange raw
      let (main, extra) = fmtStatRange sr
          unit = case mUnit of
            Just u | not (T.null (T.strip u)) -> T.strip u
            _ -> maybe "" id (ctxUnits ctx cid)
      pure (main, unit, extra ++ commentRemark mc ++ originRemark origin)

    commentRemark = maybe [] pure
    originRemark o = case o of
      OriginInherited p           -> ["inherited from '" <> p <> "'"]
      OriginImplicitOmitted _     -> ["implicit value (*IMPLICIT VALUES)"]
      OriginImplicitUnspecified _ -> ["implicit value (*IMPLICIT VALUES)"]
      _                           -> []

-- | @description.txt@: prosa morfológica y caracteres @TE@.
dwcDescriptionTxt :: DwcOptions -> ExportContext -> Text
dwcDescriptionTxt opts ctx = tsv
  ["coreid", "type", "description", "language"]
  (concat (zipWith rowsFor (dwcTaxa ctx) (zip (ecTaxa ctx) prose)))
  where
    natOpts = (dwcNatural opts) { noIncludeText = False }
    prose = map plainDescription (describeTaxa natOpts ctx)
    lang = case noLang (dwcNatural opts) of
      LangEn -> "en"
      LangEs -> "es"
    rowsFor d (t, p) =
      [ [dtId d, "morphology", p, lang] | not (T.null p) ]
      ++ [ [dtId d, "morphology", ctxFeature ctx cid <> ": " <> T.strip txt, lang]
         | (cid, AttributedState (AttrText _ txt) _) <- Map.toAscList (rtAttributes t)
         , not (T.null (T.strip txt))
         ]

--------------------------------------------------------------------------------
-- Descriptores XML
--------------------------------------------------------------------------------

dwcTerm :: Text -> Text
dwcTerm = ("http://rs.tdwg.org/dwc/terms/" <>)

dcTerm :: Text -> Text
dcTerm = ("http://purl.org/dc/terms/" <>)

-- | @meta.xml@: descriptor del esquema en estrella (UTF-8, tabulador, 1 línea de cabecera).
dwcMetaXml :: Text
dwcMetaXml = T.unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<archive xmlns=\"http://rs.tdwg.org/dwc/text/\" metadata=\"eml.xml\">"
  ] ++ block "core" (dwcTerm "Taxon") "taxon.txt" "id"
         [(0, dwcTerm "taxonID"), (1, dwcTerm "scientificName"), (2, dwcTerm "scientificNameAuthorship")
         , (3, dwcTerm "taxonRank"), (4, dwcTerm "parentNameUsageID"), (5, dwcTerm "taxonRemarks")]
    ++ block "extension" (dwcTerm "MeasurementOrFact") "measurementorfact.txt" "coreid"
         [(1, dwcTerm "measurementType"), (2, dwcTerm "measurementValue"), (3, dwcTerm "measurementUnit")
         , (4, dwcTerm "measurementRemarks")]
    ++ block "extension" "http://rs.gbif.org/terms/1.0/Description" "description.txt" "coreid"
         [(1, dcTerm "type"), (2, dcTerm "description"), (3, dcTerm "language")]
    ++ [ "</archive>" ]
  where
    block :: Text -> Text -> Text -> Text -> [(Int, Text)] -> [Text]
    block tag rowType file keyTag fields =
      [ "  <" <> tag <> " encoding=\"UTF-8\" fieldsTerminatedBy=\"\\t\" linesTerminatedBy=\"\\n\" fieldsEnclosedBy=\"\" ignoreHeaderLines=\"1\" rowType=\"" <> rowType <> "\">"
      , "    <files>"
      , "      <location>" <> file <> "</location>"
      , "    </files>"
      , "    <" <> keyTag <> " index=\"0\"/>"
      ] ++
      [ "    <field index=\"" <> T.pack (show i) <> "\" term=\"" <> term <> "\"/>" | (i, term) <- fields ] ++
      [ "  </" <> tag <> ">" ]

-- | @eml.xml@: metadatos EML 2.1.1 (perfil GBIF) a partir de 'DatasetMetadata'.
dwcEmlXml :: Lang -> DatasetMetadata -> Text
dwcEmlXml lang meta = T.unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<eml:eml xmlns:eml=\"eml://ecoinformatics.org/eml-2.1.1\" xmlns:dc=\"http://purl.org/dc/terms/\""
  , "         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
  , "         xsi:schemaLocation=\"eml://ecoinformatics.org/eml-2.1.1 http://rs.gbif.org/schema/eml-gbif-profile/1.1/eml.xsd\""
  , "         packageId=\"" <> escapeXml packageId <> "\" system=\"https://taxide.org\" scope=\"system\" xml:lang=\"" <> code <> "\">"
  , "  <dataset>"
  ] ++ [ "    <alternateIdentifier>" <> escapeXml i <> "</alternateIdentifier>" | Just i <- [datasetIdentifier meta] ] ++
  [ "    <title xml:lang=\"" <> code <> "\">" <> escapeXml (datasetTitle meta) <> "</title>" ] ++
  map creator (creatorsOrDefault) ++
  [ "    <language>" <> code <> "</language>" ] ++
  [ "    <abstract><para>" <> escapeXml a <> "</para></abstract>" | Just a <- [datasetAbstract meta] ] ++
  [ "    <intellectualRights><para>" <> licensePara l <> "</para></intellectualRights>" | Just l <- [datasetLicense meta] ] ++
  [ contactBlock ] ++
  [ "  </dataset>" ] ++
  concat
    [ [ "  <additionalMetadata>"
      , "    <metadata>"
      , "      <gbif>"
      , "        <citation>" <> escapeXml c <> "</citation>"
      , "      </gbif>"
      , "    </metadata>"
      , "  </additionalMetadata>" ]
    | Just c <- [datasetCitation meta] ] ++
  [ "</eml:eml>" ]
  where
    code = case lang of
      LangEn -> "en"
      LangEs -> "es"
    packageId = maybe (slugify (datasetTitle meta)) id (datasetIdentifier meta)
    creatorsOrDefault = case datasetCreators meta of
      [] -> [Nothing]
      xs -> map Just xs
    creator Nothing = "    <creator><organizationName>Unspecified</organizationName></creator>"
    creator (Just n) = "    <creator><individualName><surName>" <> escapeXml n <> "</surName></individualName></creator>"
    contactBlock = case datasetContact meta of
      Nothing -> "    <contact><organizationName>Unspecified</organizationName></contact>"
      Just c
        | "@" `T.isInfixOf` c ->
            "    <contact><individualName><surName>" <> escapeXml c <> "</surName></individualName><electronicMailAddress>"
              <> escapeXml c <> "</electronicMailAddress></contact>"
        | otherwise ->
            "    <contact><individualName><surName>" <> escapeXml c <> "</surName></individualName></contact>"
    licensePara l = case find ((== T.toLower l) . T.toLower . fst) knownLicenses of
      Just (_, (url, title)) ->
        "This work is licensed under <ulink url=\"" <> url <> "\"><citetitle>" <> title <> "</citetitle></ulink>."
      Nothing -> escapeXml l
    knownLicenses =
      [ ("cc-by-4.0", ("https://creativecommons.org/licenses/by/4.0/legalcode", "Creative Commons Attribution (CC-BY) 4.0"))
      , ("cc0", ("https://creativecommons.org/publicdomain/zero/1.0/legalcode", "Creative Commons CC0 1.0 Universal"))
      , ("cc0-1.0", ("https://creativecommons.org/publicdomain/zero/1.0/legalcode", "Creative Commons CC0 1.0 Universal"))
      , ("cc-by-nc-4.0", ("https://creativecommons.org/licenses/by-nc/4.0/legalcode", "Creative Commons Attribution Non Commercial (CC-BY-NC) 4.0"))
      ]

--------------------------------------------------------------------------------
-- Empaquetado
--------------------------------------------------------------------------------

-- | Las cinco entradas del esquema en estrella, en orden fijo.
dwcFiles :: DwcOptions -> ExportContext -> [(FilePath, Text)]
dwcFiles opts ctx =
  [ ("taxon.txt", dwcTaxonTxt ctx)
  , ("measurementorfact.txt", dwcMeasurementTxt ctx)
  , ("description.txt", dwcDescriptionTxt opts ctx)
  , ("meta.xml", dwcMetaXml)
  , ("eml.xml", dwcEmlXml (noLang (dwcNatural opts)) (ecMetadata ctx))
  ]

-- | Ensambla el @.zip@ de Darwin Core Archive en memoria. Reproducible byte a
-- byte: mismo orden de entradas, marca de tiempo fija y sin metadatos del sistema.
renderDwcArchive :: DwcOptions -> ExportContext -> BL.ByteString
renderDwcArchive opts ctx =
  let entries = [ Zip.toEntry path dwcEpoch (BL.fromStrict (TE.encodeUtf8 content))
                | (path, content) <- dwcFiles opts ctx ]
  in Zip.fromArchive (foldr Zip.addEntryToArchive Zip.emptyArchive entries)
