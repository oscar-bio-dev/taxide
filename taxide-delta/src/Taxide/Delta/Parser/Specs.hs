{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Delta.Parser.Specs
Description : Parser Megaparsec para archivos de especificaciones (*SPECS) de DELTA
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0
-}

module Taxide.Delta.Parser.Specs
  ( pSpecsFile
  , parseSpecsText
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

import Taxide.Core.Specs (DependentRule (..), ImplicitRule (..), SpecsDef (..))
import Taxide.Core.Types (CharacterType (..), StateId)
import Taxide.Delta.Parser.Lexer

-- | Parsea el archivo SPECS completo hasta el final (EOF).
pSpecsFile :: Parser SpecsDef
pSpecsFile = do
  sc
  directives <- many pDirectiveEntry
  eof
  pure (foldr applyDirective defaultSpecs directives)
  where
    defaultSpecs = SpecsDef 0 0 0 Map.empty Map.empty [] []

-- | Suma de directivas soportadas en SPECS.
data SpecDirective
  = DirNumChars !Int
  | DirMaxStates !Int
  | DirMaxItems !Int
  | DirCharTypes !(Map Int CharacterType)
  | DirNumbersOfStates !(Map Int Int)
  | DirDependencies ![DependentRule]
  | DirImplicitValues ![ImplicitRule]
  | DirIgnored

-- | Aplica una directiva sobre la estructura SpecsDef.
applyDirective :: SpecDirective -> SpecsDef -> SpecsDef
applyDirective dir s = case dir of
  DirNumChars n        -> s { specNumCharacters = n }
  DirMaxStates n       -> s { specMaxNumberStates = n }
  DirMaxItems n        -> s { specMaxNumberItems = n }
  DirCharTypes m       -> s { specCharTypes = specCharTypes s <> m }
  DirNumbersOfStates m -> s { specNumbersOfStates = specNumbersOfStates s <> m }
  DirDependencies d    -> s { specDependencies = specDependencies s ++ d }
  DirImplicitValues iv -> s { specImplicitValues = specImplicitValues s ++ iv }
  DirIgnored           -> s

-- | Parsea una directiva individual en SPECS.
pDirectiveEntry :: Parser SpecDirective
pDirectiveEntry = choice
  [ try pDirNumChars
  , try pDirMaxStates
  , try pDirMaxItems
  , try pDirCharTypes
  , try pDirNumbersOfStates
  , try pDirDependencies
  , try pDirImplicitValues
  , pDirGenericIgnored
  ]

pDirNumChars :: Parser SpecDirective
pDirNumChars = do
  pDirectivePrefix ["number", "of", "characters"]
  n <- pInteger
  pure (DirNumChars n)

pDirMaxStates :: Parser SpecDirective
pDirMaxStates = do
  pDirectivePrefix ["maximum", "number", "of", "states"]
  n <- pInteger
  pure (DirMaxStates n)

pDirMaxItems :: Parser SpecDirective
pDirMaxItems = do
  pDirectivePrefix ["maximum", "number", "of", "items"]
  n <- pInteger
  pure (DirMaxItems n)

pDirCharTypes :: Parser SpecDirective
pDirCharTypes = do
  pDirectivePrefix ["character", "types"]
  entries <- many (lexeme pCharTypeMapping)
  pure (DirCharTypes (Map.fromList (concat entries)))
  where
    pCharTypeMapping = do
      chars <- pIdRange
      _ <- char ','
      ctype <- pCtype
      pure [ (c, ctype) | c <- chars ]

    pCtype = choice
      [ string' "EUM" *> pure EUM
      , string' "EOM" *> pure EOM
      , string' "UM"  *> pure UM
      , string' "OM"  *> pure OM
      , string' "IN"  *> pure IN
      , string' "RN"  *> pure RN
      , string' "TE"  *> pure TE
      ]

pDirNumbersOfStates :: Parser SpecDirective
pDirNumbersOfStates = do
  pDirectivePrefix ["numbers", "of", "states"]
  entries <- many (lexeme pNumStatesMapping)
  pure (DirNumbersOfStates (Map.fromList (concat entries)))
  where
    pNumStatesMapping = do
      chars <- pIdRange
      _ <- char ','
      n <- pInteger
      pure [ (c, n) | c <- chars ]

pDirDependencies :: Parser SpecDirective
pDirDependencies = do
  pDirectivePrefix ["dependent", "characters"]
  rules <- many (lexeme pDependentRule)
  pure (DirDependencies rules)
  where
    pDependentRule = do
      controller <- pInteger
      _ <- char ','
      disabling <- pDisablingStates
      _ <- char ':'
      dependents <- sepBy1 pIdRange (char ':')
      pure $ DependentRule
        { depControllerChar  = controller
        , depDisablingStates = disabling
        , depDependentChars  = concat dependents
        }

    pDisablingStates :: Parser (Set StateId)
    pDisablingStates = do
      states <- sepBy1 pInteger (char '/')
      pure (Set.fromList states)

pDirImplicitValues :: Parser SpecDirective
pDirImplicitValues = do
  pDirectivePrefix ["implicit", "values"]
  rules <- many (lexeme pImplicitRule)
  pure (DirImplicitValues rules)
  where
    pImplicitRule = do
      c <- pInteger
      _ <- char ','
      omittedState <- pInteger
      mUnspec <- optional (char ':' *> pInteger)
      pure $ ImplicitRule
        { impCharId      = c
        , impOmitted     = omittedState
        , impUnspecified = mUnspec
        }

-- | Ignora directivas auxiliares no críticas (ej. *DATA BUFFER SIZE, *SHOW, etc.)
pDirGenericIgnored :: Parser SpecDirective
pDirGenericIgnored = do
  _ <- char '*'
  _ <- takeWhileP (Just "ignored directive") (\c -> c /= '*' && c /= '/')
  _ <- optional (char '/')
  sc
  pure DirIgnored

-- | Parsea un rango o lista de enteros: "1" -> [1], "1-5" -> [1,2,3,4,5].
pIdRange :: Parser [Int]
pIdRange = do
  startId <- pInteger
  mEndId <- optional (char '-' *> pInteger)
  case mEndId of
    Just endId -> pure [startId .. endId]
    Nothing    -> pure [startId]

-- | Función pura para parsear una cadena de texto SPECS.
parseSpecsText :: FilePath -> Text -> Either Text SpecsDef
parseSpecsText path input =
  case runParser pSpecsFile path input of
    Left err  -> Left (T.pack (errorBundlePretty err))
    Right res -> Right res
