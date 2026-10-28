{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Suite de pruebas de taxide-export (NEXUS, TNT, prosa natural, DwC-A, JSON, binario HPC)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Main (main) where

import qualified Codec.Archive.Zip as Zip
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty
import Test.Tasty.HUnit

import Taxide.Core.Character (CharacterDef (..), mkMultistateChar, mkNumericChar, mkTextChar)
import Taxide.Core.Item (ItemDef (..), mkPrimaryItem, mkVariantItem)
import Taxide.Core.Specs (ImplicitRule (..), SpecsDef (..), emptySpecs)
import Taxide.Core.Types
  ( CharacterType (..)
  , PseudoValue (..)
  , RawAttribute (..)
  , RawStateExpr (..)
  , StateId
  , StateToken (..)
  , emptyDatasetMetadata
  , mkRangeStat
  , mkSingleStat
  )
import Taxide.Engine.Matrix (ColumnarMatrix (..))
import Taxide.Engine.Pipeline (runPipelinePure)
import Taxide.Export.BinaryHPC (HpcInfo (..), HpcSection (..), hpcAlignment, readHpcInfo, renderHpcBinary)
import Taxide.Export.Common (ExportContext (..), mkExportContext)
import Taxide.Export.DarwinCore (defaultDwcOptions, dwcFiles, renderDwcArchive)
import Taxide.Export.Json (renderJson)
import Taxide.Export.Natural
  ( NaturalOptions (..)
  , defaultNaturalOptions
  , renderNaturalHtml
  , renderNaturalMarkdown
  )
import Taxide.Export.Nexus (defaultNexusOptions, renderNexus)
import Taxide.Export.TNT (defaultTntOptions, renderTnt)

--------------------------------------------------------------------------------
-- Conjunto de datos de prueba
--------------------------------------------------------------------------------

mkState :: StateId -> RawAttribute
mkState s = AttrStates Nothing (SingleState (StateToken s Nothing))

tok :: StateId -> RawStateExpr
tok s = SingleState (StateToken s Nothing)

pseudo :: PseudoValue -> RawAttribute
pseudo p = AttrStates Nothing (PseudoState p Nothing)

specs :: SpecsDef
specs = emptySpecs
  { specNumCharacters = 4
  , specCharTypes     = Map.fromList [(1, UM), (2, OM), (3, RN), (4, TE)]
  , specImplicitValues = [ImplicitRule 1 3 Nothing]
  }

chars :: [CharacterDef]
chars =
  [ (mkMultistateChar 1 UM "wing" [(1, "short"), (2, "long"), (3, "none")]) { charHeading = Just "Wings" }
  , mkMultistateChar 2 OM "size" [(1, "small"), (2, "medium"), (3, "big")]
  , mkNumericChar 3 RN "length" (Just "mm")
  , mkTextChar 4 "notes"
  ]

items :: [ItemDef]
items =
  [ (mkPrimaryItem "Alpha"
      [ (1, AttrStates Nothing (StateAnd (tok 1) (tok 2)))     -- polimorfismo simultáneo 1&2
      , (2, AttrStates Nothing (StateOr (tok 1) (tok 2)))      -- incertidumbre 1/2
      , (3, AttrReal Nothing (mkRangeStat 10 12) Nothing)
      , (4, AttrText Nothing "Found & seen <often>")
      ]) { itemAuthority = Just "Smith, 1901" }
  , mkVariantItem "Alpha var. major" [ (2, mkState 3) ]
  , mkVariantItem "Alpha male" [ (2, mkState 2) ]
  , mkPrimaryItem "Beta"
      [ (1, mkState 3), (2, mkState 3), (3, AttrReal Nothing (mkSingleStat 20) Nothing) ]
  , mkPrimaryItem "Gamma"
      [ (1, pseudo Unknown), (2, mkState 1), (3, AttrReal Nothing (mkSingleStat 30) Nothing) ]
  , mkPrimaryItem "Delta" [ (2, mkState 2), (3, AttrReal Nothing (mkSingleStat 15) Nothing) ]  -- carácter 1 implícito
  ]

mkCtx :: ExportContext
mkCtx =
  let (taxa, mat, _) = runPipelinePure specs items
  in mkExportContext (emptyDatasetMetadata "Prueba <TAXIDE> & co") specs chars taxa mat

--------------------------------------------------------------------------------
-- Auxiliares
--------------------------------------------------------------------------------

-- | Filas de la MATRIX NEXUS: (etiqueta, datos).
nexusRows :: Text -> [(Text, Text)]
nexusRows doc =
  [ (T.unwords (init ws), last ws)
  | l <- takeWhile (/= "  ;") (drop 1 (dropWhile (\x -> T.strip x /= "MATRIX") (T.lines doc)))
  , let ws = T.words l
  , not (null ws)
  ]

-- | Filas TSV (sin cabecera) de un texto.
tsvRows :: Text -> [[Text]]
tsvRows = map (T.splitOn "\t") . drop 1 . T.lines

-- | Primera columna de una fila TSV (vacía si la fila está vacía).
col0 :: [Text] -> Text
col0 r = case r of
  (x : _) -> x
  []      -> ""

zipText :: Zip.Archive -> FilePath -> Maybe Text
zipText arch path = do
  e <- Zip.findEntryByPath path arch
  pure (TE.decodeUtf8 (BL.toStrict (Zip.fromEntry e)))

--------------------------------------------------------------------------------
-- Pruebas
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "Taxide.Export Test Suite"
  [ nexusTests, tntTests, naturalTests, dwcaTests, jsonTests, hpcTests ]

nexusTests :: TestTree
nexusTests = testGroup "NEXUS"
  [ testCase "dimensiones, bloques y símbolos exactos" $ do
      doc <- either (assertFailure . show) pure (renderNexus defaultNexusOptions mkCtx)
      let ls = map T.strip (T.lines doc)
      assertBool "cabecera #NEXUS" (take 1 ls == ["#NEXUS"])
      mapM_ (\b -> assertBool (T.unpack b) (b `elem` ls))
        ["BEGIN TAXA;", "BEGIN CHARACTERS;", "BEGIN ASSUMPTIONS;", "DIMENSIONS NTAX=6;", "DIMENSIONS NCHAR=3;"]
      assertBool "FORMAT estándar" ("FORMAT DATATYPE=STANDARD SYMBOLS=\"012\" MISSING=? GAP=-;" `elem` ls)
      assertBool "CHARLABELS" ("CHARLABELS" `elem` ls)
      assertBool "STATELABELS" ("STATELABELS" `elem` ls)

  , testCase "MATRIX distingue polimorfismo (12) de incertidumbre {12}" $ do
      doc <- either (assertFailure . show) pure (renderNexus defaultNexusOptions mkCtx)
      let rows = nexusRows doc
      length rows @?= 6
      -- Alpha: 1&2 -> (01), 1/2 -> {01}, 10-12 -> clase 0
      lookup "'Alpha'" rows @?= Just "(01){01}0"
      -- 6 taxones, k = ceil(sqrt 6) = 3 clases sobre [11, 30]: Beta (20) -> 1; Gamma (30) -> 2
      lookup "'Beta'" rows @?= Just "221"
      lookup "'Gamma'" rows @?= Just "?02"
      -- Todas las filas tienen exactamente NCHAR celdas lógicas (contando (..) y {..} como una)
      mapM_ (\(_, r) -> assertEqual "celdas por fila" 3 (cells r)) rows

  , testCase "TYPESET: unord para UM y ord para OM y cuantitativos discretizados" $ do
      doc <- either (assertFailure . show) pure (renderNexus defaultNexusOptions mkCtx)
      assertBool "typeset" ("TYPESET taxide_types = unord: 1, ord: 2-3;" `elem` map T.strip (T.lines doc))
  ]
  where
    cells :: Text -> Int
    cells = go 0 . T.unpack
      where
        go n [] = n
        go n ('(' : r) = go (n + 1) (drop 1 (dropWhile (/= ')') r))
        go n ('{' : r) = go (n + 1) (drop 1 (dropWhile (/= '}') r))
        go n (_ : r) = go (n + 1) r

tntTests :: TestTree
tntTests = testGroup "TNT"
  [ testCase "xread con dimensiones exactas, polimorfismos [..], cnames y ccode" $ do
      doc <- either (assertFailure . show) pure (renderTnt defaultTntOptions mkCtx)
      let ls = map T.strip (T.lines doc)
      assertBool "nstates" ("nstates 32;" `elem` ls)
      assertBool "xread" ("xread" `elem` ls)
      assertBool "nchar ntax" ("3 6" `elem` ls)
      assertBool "polimorfismo/incertidumbre entre corchetes" ("Alpha [01][01]0" `elem` ls)
      assertBool "Beta" ("Beta 221" `elem` ls)
      assertBool "Gamma con faltante" ("Gamma ?02" `elem` ls)
      assertBool "ccode: + ordenados, - desordenados" ("ccode + 1 2 - 0;" `elem` ls)
      assertBool "cnames con estados" ("{0 wing short long none;" `elem` ls)
      assertBool "fin" ("proc /;" `elem` ls)
  ]

naturalTests :: TestTree
naturalTests = testGroup "Lenguaje natural"
  [ testCase "Markdown: subtítulos, autoría, variantes, unidades y texto" $ do
      let md = renderNaturalMarkdown defaultNaturalOptions mkCtx
          mdLower = T.toLower md
      assertBool "taxón con autoría" ("## *Alpha* Smith, 1901" `T.isInfixOf` md)
      assertBool "subtítulo temático" ("### Wings" `T.isInfixOf` md)
      assertBool "polimorfismo y incertidumbre" ("wing short and long" `T.isInfixOf` mdLower)
      assertBool "unidades" ("length 10-12 mm" `T.isInfixOf` mdLower)
      assertBool "variante" ("Variant of Alpha." `T.isInfixOf` md)
      assertBool "texto libre" ("Notes: Found & seen <often>." `T.isInfixOf` md)

  , testCase "Omisión de valores implícitos y de unidades/texto" $ do
      let withImp = renderNaturalMarkdown defaultNaturalOptions mkCtx
          noImp = renderNaturalMarkdown defaultNaturalOptions { noOmitImplicit = True } mkCtx
          bare = renderNaturalMarkdown defaultNaturalOptions { noShowUnits = False, noIncludeText = False } mkCtx
      -- Delta carece del carácter 1: el implícito (3 = none) sólo aparece sin omisión
      let count = T.count "wing none" . T.toLower
      assertBool "implícito presente" (count withImp >= 1)
      assertBool "implícito omitido" (count noImp == count withImp - 1)
      assertBool "sin unidades" (not ("10-12 mm" `T.isInfixOf` bare))
      assertBool "sin texto" (not ("Found" `T.isInfixOf` bare))

  , testCase "HTML5 escapa caracteres reservados" $ do
      let html = renderNaturalHtml defaultNaturalOptions mkCtx
      assertBool "doctype" ("<!DOCTYPE html>" `T.isPrefixOf` html)
      assertBool "título escapado" ("Prueba &lt;TAXIDE&gt; &amp; co" `T.isInfixOf` html)
      assertBool "texto escapado" ("Found &amp; seen &lt;often&gt;" `T.isInfixOf` html)
      assertBool "encabezado" ("<h2><i>Alpha</i> Smith, 1901</h2>" `T.isInfixOf` html)
  ]

dwcaTests :: TestTree
dwcaTests = testGroup "Darwin Core Archive"
  [ testCase "el ZIP es determinista (mismos bytes en ejecuciones independientes)" $ do
      let build = renderDwcArchive defaultDwcOptions
          a = build mkCtx
          b = build mkCtx
          c = build (let (taxa, mat, _) = runPipelinePure specs items
                     in mkExportContext (emptyDatasetMetadata "Prueba <TAXIDE> & co") specs chars taxa mat)
      assertBool "no vacío" (BL.length a > 0)
      a @?= b
      a @?= c

  , testCase "contiene exactamente las 5 entradas del esquema en estrella, en orden fijo" $ do
      let arch = Zip.toArchive (renderDwcArchive defaultDwcOptions mkCtx)
      Zip.filesInArchive arch @?=
        ["taxon.txt", "measurementorfact.txt", "description.txt", "meta.xml", "eml.xml"]
      assertEqual "marca de tiempo fija" [315532800]
        (Set.toList (Set.fromList (map Zip.eLastModified (Zip.zEntries arch))))

  , testCase "measurementorfact.txt no contiene U ni -" $ do
      let arch = Zip.toArchive (renderDwcArchive defaultDwcOptions mkCtx)
      mof <- maybe (assertFailure "falta measurementorfact.txt") pure (zipText arch "measurementorfact.txt")
      let rows = tsvRows mof
      assertBool "hay registros" (not (null rows))
      mapM_ (\r -> assertEqual "5 columnas" 5 (length r)) rows
      mapM_ (\r -> assertBool ("valor " ++ show (r !! 2)) (r !! 2 `notElem` ["U", "-", ""])) rows
      -- Gamma tiene el carácter 1 desconocido: no debe generar registro para 'wing'
      assertBool "sin fila para celda desconocida"
        (null [ () | r <- rows, r !! 0 == "gamma", r !! 1 == "wing" ])

  , testCase "todas las claves foráneas coreid resuelven en taxon.txt" $ do
      let arch = Zip.toArchive (renderDwcArchive defaultDwcOptions mkCtx)
      taxon <- maybe (assertFailure "falta taxon.txt") pure (zipText arch "taxon.txt")
      let ids = Set.fromList [r !! 0 | r <- tsvRows taxon]
          check f = do
            t <- maybe (assertFailure ("falta " ++ f)) pure (zipText arch f)
            mapM_ (\r -> let k = col0 r in assertBool ("coreid huérfano: " ++ T.unpack k) (k `Set.member` ids)) (tsvRows t)
      Set.size ids @?= 6
      check "measurementorfact.txt"
      check "description.txt"

  , testCase "variantes: parentNameUsageID hacia π(i); taxonRank sólo con marcador explícito" $ do
      let arch = Zip.toArchive (renderDwcArchive defaultDwcOptions mkCtx)
      taxon <- maybe (assertFailure "falta taxon.txt") pure (zipText arch "taxon.txt")
      let rows = Map.fromList [(r !! 1, r) | r <- tsvRows taxon]
          get n = maybe (error (T.unpack n)) id (Map.lookup n rows)
          major = get "Alpha var. major"
          male  = get "Alpha male"
      -- columnas: id, nombre, autoría, rango, padre, observaciones
      major !! 4 @?= "alpha"
      major !! 3 @?= "variety"
      male !! 4 @?= "alpha"
      male !! 3 @?= ""
      assertBool "documentado en taxonRemarks" ("not specified" `T.isInfixOf` (male !! 5))
      get "Alpha" !! 2 @?= "Smith, 1901"
      get "Alpha" !! 4 @?= ""

  , testCase "meta.xml y eml.xml bien formados y coherentes" $ do
      let files = Map.fromList (dwcFiles defaultDwcOptions mkCtx)
          meta = files Map.! "meta.xml"
          eml = files Map.! "eml.xml"
      mapM_ (\t -> assertBool (T.unpack t) (t `T.isInfixOf` meta))
        [ "http://rs.tdwg.org/dwc/terms/Taxon", "http://rs.tdwg.org/dwc/terms/MeasurementOrFact"
        , "http://rs.gbif.org/terms/1.0/Description", "<coreid index=\"0\"/>", "fieldsTerminatedBy=\"\\t\""
        , "http://rs.tdwg.org/dwc/terms/parentNameUsageID" ]
      assertBool "título escapado en EML" ("Prueba &lt;TAXIDE&gt; &amp; co" `T.isInfixOf` eml)
      assertBool "licencia" ("CC-BY" `T.isInfixOf` eml)
  ]

jsonTests :: TestTree
jsonTests = testCase "JSON: FeatureCollection con una Feature por taxón" $ do
  v <- maybe (assertFailure "JSON inválido") pure (decode (renderJson mkCtx) :: Maybe Value)
  case v of
    Object o -> do
      KM.lookup "type" o @?= Just (String "FeatureCollection")
      case KM.lookup "features" o of
        Just (Array fs) -> length fs @?= 6
        _ -> assertFailure "faltan features"
    _ -> assertFailure "se esperaba un objeto"

hpcTests :: TestTree
hpcTests = testGroup "Binario HPC"
  [ testCase "cabecera válida; tensores y tamaño total alineados a 64 bytes" $ do
      let bs = renderHpcBinary mkCtx
      info' <- either (assertFailure . T.unpack) pure (readHpcInfo bs)
      hiTaxa info' @?= 6
      hiChars info' @?= 4
      fromIntegral (BL.length bs) `mod` hpcAlignment @?= 0
      fromIntegral (hiHeader info') `mod` hpcAlignment @?= 0
      assertBool "hay secciones" (not (null (hiSections info')))
      mapM_ (\s -> assertEqual "desplazamiento alineado" 0 (fromIntegral (hsOffset s) `mod` hpcAlignment))
        (hiSections info')

  , testCase "la máscara Word64 del carácter 1 de Alpha codifica los estados {1,2}" $ do
      let bs = renderHpcBinary mkCtx
      info' <- either (assertFailure . T.unpack) pure (readHpcInfo bs)
      case [s | s <- hiSections info', hsKind s == 1, hsChar s == 1] of
        (s : _) -> do
          let w = BL.take 8 (BL.drop (fromIntegral (hsOffset s)) bs)
              value = foldr (\b acc -> acc * 256 + fromIntegral b) (0 :: Integer) (BL.unpack w)
          value @?= 3
          hsRows s @?= 6
        [] -> assertFailure "falta la sección de máscaras del carácter 1"
      cmNumTaxa (ecMatrix mkCtx) @?= 6
  ]
