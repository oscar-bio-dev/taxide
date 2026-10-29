{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Export.Natural
Description : Síntesis de descripciones taxonómicas en lenguaje natural (reemplazo de TONAT)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Función pura en memoria que redacta, para cada taxón de la matriz resuelta, una
descripción en prosa (Markdown o HTML5). Soporta la omisión opcional de valores
implícitos, la inserción de unidades en @IN@/@RN@, la inclusión de caracteres
@TE@ y la agrupación por subtítulos temáticos (los encabezados @<...>@ de CHARS,
equivalentes a @*ITEM SUBHEADINGS@).
-}

module Taxide.Export.Natural
  ( Lang (..)
  , NaturalOptions (..)
  , defaultNaturalOptions
  , NaturalSection (..)
  , NaturalTaxon (..)
  , describeTaxa
  , plainDescription
  , renderNaturalMarkdown
  , renderNaturalHtml
  ) where

#if __GLASGOW_HASKELL__ < 910
import Data.List (foldl')
#endif
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
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
  , StateToken (..)
  , CharacterType (..)
  )
import Taxide.Engine.Discretize (quantitativeRange)
import Taxide.Engine.Variants (AttrOrigin (..), AttributedState (..), ResolvedTaxon (..))
import Taxide.Export.Common

-- | Idioma de la prosa generada.
data Lang = LangEn | LangEs
  deriving (Eq, Show)

-- | Opciones de síntesis.
data NaturalOptions = NaturalOptions
  { noLang         :: !Lang   -- ^ Idioma de las partículas de enlace
  , noOmitImplicit :: !Bool   -- ^ Omite valores provenientes de @*IMPLICIT VALUES@
  , noShowUnits    :: !Bool   -- ^ Inserta unidades en caracteres @IN@/@RN@
  , noIncludeText  :: !Bool   -- ^ Incluye caracteres @TE@ como oraciones propias
  , noGroupHeadings :: !Bool  -- ^ Agrupa por subtítulos temáticos de CHARS
  } deriving (Eq, Show)

-- | Inglés, unidades visibles, texto incluido y agrupación activa.
defaultNaturalOptions :: NaturalOptions
defaultNaturalOptions = NaturalOptions
  { noLang          = LangEn
  , noOmitImplicit  = False
  , noShowUnits     = True
  , noIncludeText   = True
  , noGroupHeadings = True
  }

-- | Bloque temático de la descripción de un taxón.
data NaturalSection = NaturalSection
  { nsHeading   :: !(Maybe Text)
  , nsSentences :: ![Text]
  } deriving (Eq, Show)

-- | Descripción completa de un taxón.
data NaturalTaxon = NaturalTaxon
  { ntIndex     :: !Int
  , ntName      :: !Text
  , ntAuthority :: !(Maybe Text)
  , ntVariantOf :: !(Maybe Text)  -- ^ Nombre del progenitor si es variante @#+@
  , ntSections  :: ![NaturalSection]
  } deriving (Eq, Show)

-- | Partículas de enlace por idioma.
data Words' = Words'
  { wOr, wAnd, wTo, wBetween, wVariable, wVariantOf, wCentre, wExtremes, wMin, wMax :: Text }

wordsFor :: Lang -> Words'
wordsFor LangEn = Words' "or" "and" "to" "intermediate between" "variable" "Variant of" "centre" "extremes" "minimum" "maximum"
wordsFor LangEs = Words' "o" "y" "a" "intermedio entre" "variable" "Variante de" "centro" "extremos" "mínimo" "máximo"

-- | Fragmento de una descripción: parte de la oración principal o texto libre.
data Fragment = Inline !Text | FreeText !Text

-- | Redacta la descripción de todos los taxones de la matriz.
describeTaxa :: NaturalOptions -> ExportContext -> [NaturalTaxon]
describeTaxa opts ctx = go Nothing (ecTaxa ctx)
  where
    go _ [] = []
    go lastPrimary (t : rest) =
      let parent = case rtType t of
            VariantItem -> lastPrimary
            PrimaryItem -> Nothing
          lastPrimary' = case rtType t of
            PrimaryItem -> Just (rtName t)
            VariantItem -> lastPrimary
      in NaturalTaxon
           { ntIndex     = rtIndex t
           , ntName      = rtName t
           , ntAuthority = rtAuthority t
           , ntVariantOf = parent
           , ntSections  = describeSections opts ctx t
           } : go lastPrimary' rest

-- | Secciones temáticas de un taxón.
describeSections :: NaturalOptions -> ExportContext -> ResolvedTaxon -> [NaturalSection]
describeSections opts ctx t =
  let entries = [ (cid, heading cid, frag)
                | (cid, as) <- Map.toAscList (rtAttributes t)
                , Just frag <- [fragmentFor opts ctx cid as]
                ]
      heading cid
        | noGroupHeadings opts = ctxCharDef ctx cid >>= charHeading
        | otherwise            = Nothing
      -- Agrupación estable por subtítulo en orden de primera aparición.
      order = foldl' (\acc (_, h, _) -> if h `elem` acc then acc else acc ++ [h]) [] entries
      grouped = [ (h, [f | (_, h', f) <- entries, h' == h]) | h <- order ]
  in mapMaybe (\(h, fs) -> toSection h fs) grouped
  where
    toSection h fs =
      let inlines = [x | Inline x <- fs]
          texts = [x | FreeText x <- fs]
          main = [ capitalizeFirst (T.intercalate "; " inlines) <> "." | not (null inlines) ]
          sentences = main ++ texts
      in if null sentences then Nothing else Just (NaturalSection h sentences)

-- | Fragmento correspondiente a un atributo resuelto, si procede.
fragmentFor :: NaturalOptions -> ExportContext -> Int -> AttributedState -> Maybe Fragment
fragmentFor opts ctx cid (AttributedState raw origin)
  | noOmitImplicit opts && isImplicit origin = Nothing
  | otherwise = case raw of
      AttrStates mc expr -> case expr of
        PseudoState Unknown _      -> Nothing
        PseudoState Inapplicable _ -> Nothing
        PseudoState Variable _     -> Just (Inline (feature <> " " <> wVariable w))
        _ -> Just (Inline (feature <> " " <> renderExpr expr <> maybe "" (\c -> " (" <> c <> ")") mc))
      AttrInteger _ _ -> numeric Nothing
      AttrReal _ _ u  -> numeric u
      AttrText _ txt
        | noIncludeText opts && not (T.null (T.strip txt)) ->
            Just (FreeText (capitalizeFirst feature <> ": " <> endSentence (T.strip txt)))
        | otherwise -> Nothing
  where
    w = wordsFor (noLang opts)
    feature = ctxFeature ctx cid
    isImplicit o = case o of
      OriginImplicitOmitted _     -> True
      OriginImplicitUnspecified _ -> True
      _                           -> False
    ordered = ctxCharType ctx cid `elem` [OM, EOM]
    label s = ctxStateLabel ctx cid s

    renderExpr e = case e of
      SingleState tok -> labelTok tok
      RangeTo a b
        | ordered   -> labelTok a <> " " <> wTo w <> " " <> labelTok b
        | otherwise -> wBetween w <> " " <> labelTok a <> " " <> wAnd w <> " " <> labelTok b
      StateOr a b  -> renderExpr a <> " " <> wOr w <> " " <> renderExpr b
      StateAnd a b -> renderExpr a <> " " <> wAnd w <> " " <> renderExpr b
      PseudoState _ _ -> ""
    labelTok tok = label (stId tok) <> maybe "" (\c -> " (" <> c <> ")") (stComment tok)

    numeric mUnit = do
      sr <- quantitativeRange raw
      let unit = if noShowUnits opts then mUnit' mUnit else Nothing
          lo = srNormalMin sr
          hi = srNormalMax sr
          main = if lo == hi then fmtNum lo else fmtNum lo <> "-" <> fmtNum hi
          extra = [ wCentre w <> " " <> fmtNum c | Just c <- [srCenterVal sr] ]
               ++ extremes sr
          unitTxt = maybe "" (" " <>) unit
          extraTxt = if null extra then "" else " (" <> T.intercalate "; " extra <> ")"
      pure (Inline (feature <> " " <> main <> unitTxt <> extraTxt))
    mUnit' u = case u of
      Just x | not (T.null (T.strip x)) -> Just (T.strip x)
      _ -> ctxUnits ctx cid
    extremes sr = case (srAbsoluteMin sr, srAbsoluteMax sr) of
      (Nothing, Nothing) -> []
      (Just a, Just b)   -> [wExtremes w <> " " <> fmtNum a <> "-" <> fmtNum b]
      (Just a, Nothing)  -> [wMin w <> " " <> fmtNum a]
      (Nothing, Just b)  -> [wMax w <> " " <> fmtNum b]

-- | Asegura que el texto libre termine en signo de puntuación.
endSentence :: Text -> Text
endSentence t
  | T.null t = t
  | T.last t `elem` (".!?" :: String) = t
  | otherwise = t <> "."

-- | Descripción en texto plano de un taxón (una línea por sección).
plainDescription :: NaturalTaxon -> Text
plainDescription nt = T.intercalate " " (map renderSection (ntSections nt))
  where
    renderSection s = case nsHeading s of
      Just h  -> h <> ": " <> T.unwords (nsSentences s)
      Nothing -> T.unwords (nsSentences s)

-- | Texto de cabecera de un taxón: nombre y autoridad.
heading' :: NaturalTaxon -> (Text, Maybe Text)
heading' nt = (ntName nt, ntAuthority nt)

-- | Documento Markdown con la descripción de todos los taxones.
renderNaturalMarkdown :: NaturalOptions -> ExportContext -> Text
renderNaturalMarkdown opts ctx =
  T.unlines (["# " <> mdEscape (datasetTitle (ecMetadata ctx)), ""] ++ concatMap block (describeTaxa opts ctx))
  where
    w = wordsFor (noLang opts)
    block nt =
      let (name, auth) = heading' nt
      in [ "## *" <> mdEscape name <> "*" <> maybe "" (\a -> " " <> mdEscape a) auth, "" ]
         ++ [ "*" <> wVariantOf w <> " " <> mdEscape p <> ".*" <> "\n" | Just p <- [ntVariantOf nt] ]
         ++ concatMap section (ntSections nt)
    section s =
      [ "### " <> mdEscape h <> "\n" | Just h <- [nsHeading s] ]
      ++ [ mdEscape (T.unwords (nsSentences s)), "" ]

-- | Escapa los metacaracteres de Markdown más comunes.
mdEscape :: Text -> Text
mdEscape = T.concatMap esc
  where
    esc c | c `elem` ("\\*_[]`" :: String) = T.pack ['\\', c]
          | otherwise = T.singleton c

-- | Documento HTML5 con la descripción de todos los taxones.
renderNaturalHtml :: NaturalOptions -> ExportContext -> Text
renderNaturalHtml opts ctx =
  T.unlines $
    [ "<!DOCTYPE html>"
    , "<html lang=\"" <> langCode <> "\">"
    , "<head>"
    , "<meta charset=\"utf-8\">"
    , "<meta name=\"generator\" content=\"TAXIDE taxide-compile\">"
    , "<title>" <> escapeHtml title <> "</title>"
    , "</head>"
    , "<body>"
    , "<h1>" <> escapeHtml title <> "</h1>"
    ] ++ concatMap block (describeTaxa opts ctx) ++
    [ "</body>", "</html>" ]
  where
    title = datasetTitle (ecMetadata ctx)
    w = wordsFor (noLang opts)
    langCode = case noLang opts of
      LangEn -> "en"
      LangEs -> "es"
    block nt =
      let (name, auth) = heading' nt
      in [ "<article id=\"taxon-" <> T.pack (show (ntIndex nt)) <> "\">"
         , "<h2><i>" <> escapeHtml name <> "</i>" <> maybe "" (\a -> " " <> escapeHtml a) auth <> "</h2>"
         ]
         ++ [ "<p class=\"variant\"><em>" <> wVariantOf w <> " " <> escapeHtml p <> ".</em></p>" | Just p <- [ntVariantOf nt] ]
         ++ concatMap section (ntSections nt)
         ++ [ "</article>" ]
    section s =
      [ "<h3>" <> escapeHtml h <> "</h3>" | Just h <- [nsHeading s] ]
      ++ [ "<p>" <> escapeHtml (T.unwords (nsSentences s)) <> "</p>" ]
