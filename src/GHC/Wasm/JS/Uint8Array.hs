{-# LANGUAGE MagicHash #-}

module GHC.Wasm.JS.Uint8Array
  ( JSUint8Array (..),
    fromStrictByteString,
    toStrictByteString,
    fromShortByteString,
    toShortByteString,
    fromStrictText,
    toStrictText,
    fromStorableVector,
    toStorableVector,
  )
where

import Data.ByteString.Internal qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Unsafe qualified as BS
import Data.Primitive hiding (sizeOf)
import Data.Text.Internal qualified as T
import Data.Vector.Storable qualified as SV
import Foreign
import GHC.Wasm.Prim
import System.IO.Unsafe

newtype JSUint8Array = JSUint8Array {unJSUint8Array :: JSVal}

fromStrictByteString :: BS.ByteString -> JSUint8Array
fromStrictByteString bs =
  unsafePerformIO $ BS.unsafeUseAsCStringLen bs $ uncurry uint8ArrayFromPtr

toStrictByteString :: JSUint8Array -> BS.ByteString
toStrictByteString src_buf = case byteLength src_buf of
  0 -> BS.empty
  len -> BS.unsafeCreate len $ \ptr -> memsetPtr ptr len src_buf

fromShortByteString :: SBS.ShortByteString -> JSUint8Array
fromShortByteString (SBS.ShortByteString ba) =
  fromStrictText $ T.Text ba 0 $ sizeofByteArray ba

toShortByteString :: JSUint8Array -> SBS.ShortByteString
toShortByteString src_buf = SBS.ShortByteString ba
  where
    T.Text ba _ _ = toStrictText src_buf

fromStrictText :: T.Text -> JSUint8Array
fromStrictText (T.Text (ByteArray ba#) off len) =
  uint8ArrayFromByteArray# ba# off len

toStrictText :: JSUint8Array -> T.Text
toStrictText src_buf = case byteLength src_buf of
  0 -> T.empty
  len -> unsafePerformIO $ do
    mba@(MutableByteArray mba#) <- newByteArray len
    memsetMutableByteArray# mba# len src_buf
    ba <- unsafeFreezeByteArray mba
    pure $ T.Text ba 0 len

fromStorableVector :: forall a. (Storable a) => SV.Vector a -> JSUint8Array
fromStorableVector v =
  fromStrictByteString $
    BS.fromForeignPtr0 (castForeignPtr fptr) (sizeOf (undefined :: a) * len')
  where
    (fptr, len') = SV.unsafeToForeignPtr0 v

toStorableVector :: forall a. (Storable a) => JSUint8Array -> SV.Vector a
toStorableVector src_buf =
  SV.unsafeFromForeignPtr0
    (castForeignPtr fptr)
    (len `div` sizeOf (undefined :: a))
  where
    (fptr, len) = BS.toForeignPtr0 $ toStrictByteString src_buf

foreign import javascript unsafe "$1.byteLength"
  byteLength ::
    JSUint8Array -> Int

foreign import javascript unsafe "new Uint8Array(new Uint8Array(__exports.memory.buffer, $1, $2))"
  uint8ArrayFromPtr ::
    Ptr a -> Int -> IO JSUint8Array

foreign import javascript unsafe "(new Uint8Array(__exports.memory.buffer, $1, $2)).set($3)"
  memsetPtr ::
    Ptr a -> Int -> JSUint8Array -> IO ()

foreign import javascript unsafe "new Uint8Array(new Uint8Array(__exports.memory.buffer, $1 + $2, $3))"
  uint8ArrayFromByteArray# ::
    ByteArray# -> Int -> Int -> JSUint8Array

foreign import javascript unsafe "(new Uint8Array(__exports.memory.buffer, $1, $2)).set($3)"
  memsetMutableByteArray# ::
    MutableByteArray# s -> Int -> JSUint8Array -> IO ()
