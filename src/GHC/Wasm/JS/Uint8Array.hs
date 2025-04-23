module GHC.Wasm.JS.Uint8Array (
  JSUint8Array (..),
  fromStrictByteString,
  toStrictByteString,
  fromStrictText,
  toStrictText,
) where

import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Primitive
import qualified Data.Text as T
import qualified Data.Text.Extra as T
import GHC.Wasm.Prim
import System.IO.Unsafe

newtype JSUint8Array = JSUint8Array { unJSUint8Array :: JSVal }

foreign import javascript unsafe
  "$1.byteLength"
  byteLength :: JSUint8Array -> Int

fromStrictByteString :: BS.ByteString -> JSUint8Array
fromStrictByteString bs = unsafePerformIO $ BS.unsafeUseAsCStringLen bs $ uncurry uint8ArrayFromMemory

toStrictByteString :: JSUint8Array -> BS.ByteString
toStrictByteString src_buf = case byteLength src_buf of
  0 -> BS.empty
  len -> BS.unsafeCreate len $ \ptr -> memorySetUint8Array ptr len src_buf

fromStrictText :: T.Text -> JSUint8Array
fromStrictText t = T.unsafeUse t uint8ArrayFromMemory

toStrictText :: JSUint8Array -> T.Text
toStrictText src_buf = T.unsafeCreate len $
  \ptr -> memorySetUint8Array ptr len src_buf
  where
    len = byteLength src_buf

foreign import javascript unsafe
  "(new Uint8Array(__exports.memory.buffer, $1, $2)).set($3)"
  memorySetUint8Array :: Ptr a -> Int -> JSUint8Array -> IO ()

foreign import javascript unsafe
  "new Uint8Array(new Uint8Array(__exports.memory.buffer, $1, $2))"
  uint8ArrayFromMemory :: Ptr a -> Int -> IO JSUint8Array
