{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Taxide.Export.BinaryHPC
Description : Serializador binario columnar denso alineado a 64 bytes (carga zero-copy)
Copyright   : (c) TAXIDE Architecture Team, 2026
License     : Apache-2.0

Contenedor binario (@--target hdf5@) pensado para @mmap@ y lectura /zero-copy/ con
AVX-512 desde motores externos Fortran/C++. __No es un fichero HDF5__: es un
contenedor propio y mínimo cuyo diseño es compatible con volcarse luego a HDF5.

Todos los enteros y @Double@ son /little-endian/. Estructura:

* Cabecera de 64 bytes: magia @TAXIHPC1@, versión (@u32@), marca de endianness
  @0x01020304@ (@u32@), alineación, nº de taxones, nº de caracteres, nº de
  secciones, tamaño de cabecera y tamaño total (@u64@).
* Tabla de secciones: una entrada de 64 bytes por tensor (tipo, tipo de dato,
  carácter, filas, columnas, desplazamiento y longitud en bytes).
* Tensores contiguos, cada uno en un desplazamiento múltiplo de 64 y rellenado
  con ceros hasta el siguiente múltiplo de 64: máscaras @Word64@ de estados,
  etiquetas @Word8@ de pseudo-valores, cinco vectores @Double@ por carácter
  cuantitativo, la tabla de tipos de carácter y los nombres de taxón (UTF-8
  separados por NUL).
-}

module Taxide.Export.BinaryHPC
  ( hpcMagic
  , hpcAlignment
  , HpcSection (..)
  , HpcInfo (..)
  , renderHpcBinary
  , readHpcInfo
  ) where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word64)

import Taxide.Core.Types (CharacterType (..))
import Taxide.Engine.Matrix (ColumnarMatrix (..), NumericColumn (..))
import Taxide.Export.Common (ExportContext (..))

-- | Firma del formato.
hpcMagic :: BL.ByteString
hpcMagic = "TAXIHPC1"

-- | Alineación de cabecera y tensores, en bytes (línea de caché / vector ZMM).
hpcAlignment :: Int
hpcAlignment = 64

-- | Descriptor de un tensor dentro del contenedor.
data HpcSection = HpcSection
  { hsKind   :: !Word32  -- ^ 1 máscaras, 2 etiquetas discretas, 3-7 Double, 8 etiquetas numéricas, 9 nombres, 10 tipos
  , hsDType  :: !Word32  -- ^ 1 u64, 2 u8, 3 f64, 4 utf8
  , hsChar   :: !Word64  -- ^ Carácter asociado (0 si no aplica)
  , hsRows   :: !Word64
  , hsCols   :: !Word64
  , hsOffset :: !Word64  -- ^ Desplazamiento (múltiplo de 64)
  , hsLength :: !Word64  -- ^ Longitud útil en bytes (sin relleno)
  } deriving (Eq, Show)

-- | Información de cabecera del contenedor.
data HpcInfo = HpcInfo
  { hiTaxa     :: !Word64
  , hiChars    :: !Word64
  , hiHeader   :: !Word64
  , hiTotal    :: !Word64
  , hiSections :: ![HpcSection]
  } deriving (Eq, Show)

-- | Sección pendiente de ubicar: descriptor sin desplazamiento y su carga útil.
data Pending = Pending
  { pKind :: !Word32
  , pDType :: !Word32
  , pChar :: !Word64
  , pRows :: !Word64
  , pCols :: !Word64
  , pPayload :: !BL.ByteString
  }

padTo :: Int -> Int
padTo n = ((n + hpcAlignment - 1) `div` hpcAlignment) * hpcAlignment

typeCode :: CharacterType -> Int
typeCode t = case t of
  UM -> 0; OM -> 1; EUM -> 2; EOM -> 3; IN -> 4; RN -> 5; TE -> 6

-- | Serializa la matriz columnar al contenedor binario alineado a 64 bytes.
renderHpcBinary :: ExportContext -> BL.ByteString
renderHpcBinary ctx = B.toLazyByteString (header <> table <> mconcat (map body placed))
  where
    mat = ecMatrix ctx
    nTaxa = cmNumTaxa mat
    nChars = cmNumChars mat

    pendings = typeSection : namesSection : concat discreteSections ++ concat numericSections

    typeSection = Pending 10 2 0 (fromIntegral nChars) 1
      (B.toLazyByteString (mconcat
        [ B.word8 (maybe 255 (fromIntegral . typeCode) (Map.lookup c (cmCharTypes mat)))
        | c <- [1 .. nChars] ]))

    namesSection =
      let bytes = BL.fromStrict (TE.encodeUtf8 (T.intercalate "\0" (V.toList (cmTaxonNames mat)) <> "\0"))
      in Pending 9 4 0 (fromIntegral nTaxa) 1 bytes

    discreteSections =
      [ [ Pending 1 1 (fromIntegral cid) (fromIntegral nTaxa) 1
            (B.toLazyByteString (VU.foldr (\w acc -> B.word64LE w <> acc) mempty col))
        , Pending 2 2 (fromIntegral cid) (fromIntegral nTaxa) 1
            (B.toLazyByteString (VU.foldr (\w acc -> B.word8 w <> acc) mempty
               (Map.findWithDefault VU.empty cid (cmDiscreteTags mat))))
        ]
      | (cid, col) <- Map.toAscList (cmDiscreteCols mat)
      ]

    numericSections =
      [ [ doubles 3 cid (ncAbsMin nc), doubles 4 cid (ncNormalLow nc), doubles 5 cid (ncCenter nc)
        , doubles 6 cid (ncNormalHigh nc), doubles 7 cid (ncAbsMax nc)
        , Pending 8 2 (fromIntegral cid) (fromIntegral nTaxa) 1
            (B.toLazyByteString (VU.foldr (\w acc -> B.word8 w <> acc) mempty (ncPseudoTags nc)))
        ]
      | (cid, nc) <- Map.toAscList (cmNumericCols mat)
      ]

    doubles kind cid vec = Pending kind 3 (fromIntegral cid) (fromIntegral nTaxa) 1
      (B.toLazyByteString (VU.foldr (\d acc -> B.doubleLE d <> acc) mempty vec))

    nSections = length pendings
    headerBytes = 64 + 64 * nSections

    -- Ubicación secuencial de los tensores, cada uno en un múltiplo de 64.
    layout = scanl (\off p -> off + padTo (fromIntegral (BL.length (pPayload p)))) headerBytes pendings
    placed = zip pendings layout
    totalBytes = last layout

    sectionOf (p, off) = HpcSection (pKind p) (pDType p) (pChar p) (pRows p) (pCols p)
                           (fromIntegral off) (fromIntegral (BL.length (pPayload p)))

    header =
      B.lazyByteString hpcMagic
        <> B.word32LE 1
        <> B.word32LE 0x01020304
        <> B.word64LE (fromIntegral hpcAlignment)
        <> B.word64LE (fromIntegral nTaxa)
        <> B.word64LE (fromIntegral nChars)
        <> B.word64LE (fromIntegral nSections)
        <> B.word64LE (fromIntegral headerBytes)
        <> B.word64LE (fromIntegral totalBytes)

    table = mconcat
      [ B.word32LE (hsKind s) <> B.word32LE (hsDType s) <> B.word64LE (hsChar s)
          <> B.word64LE (hsRows s) <> B.word64LE (hsCols s)
          <> B.word64LE (hsOffset s) <> B.word64LE (hsLength s)
          <> B.word64LE 0 <> B.word64LE 0
      | s <- map sectionOf placed ]

    body (p, _) =
      let len = fromIntegral (BL.length (pPayload p))
      in B.lazyByteString (pPayload p) <> mconcat (replicate (padTo len - len) (B.word8 0))

-- | Lee y valida la cabecera y la tabla de secciones de un contenedor.
readHpcInfo :: BL.ByteString -> Either Text HpcInfo
readHpcInfo bs
  | BL.length bs < 64 = Left "contenedor truncado: menos de 64 bytes"
  | BL.take 8 bs /= hpcMagic = Left "firma TAXIHPC1 inválida"
  | u32 12 /= 0x01020304 = Left "marca de endianness inválida"
  | BL.length bs /= fromIntegral (u64 56) = Left "longitud total no coincide con la cabecera"
  | otherwise = Right HpcInfo
      { hiTaxa = u64 24
      , hiChars = u64 32
      , hiHeader = u64 48
      , hiTotal = u64 56
      , hiSections = [ section (64 + 64 * i) | i <- [0 .. fromIntegral (u64 40) - 1] ]
      }
  where
    byte i = fromIntegral (BL.index bs (fromIntegral (i :: Int))) :: Word64
    u32 o = fromIntegral (foldr (\k acc -> (acc `shiftL` 8) .|. byte (o + k)) 0 [0 .. 3]) :: Word32
    u64 o = foldr (\k acc -> (acc `shiftL` 8) .|. byte (o + k)) 0 [0 .. 7]
    section o = HpcSection
      { hsKind = u32 o, hsDType = u32 (o + 4), hsChar = u64 (o + 8), hsRows = u64 (o + 16)
      , hsCols = u64 (o + 24), hsOffset = u64 (o + 32), hsLength = u64 (o + 40) }
