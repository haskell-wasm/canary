module GHC.Wasm.JS.String (
  JSString (..),
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

fromStrictByteString :: BS.ByteString -> JSString
fromStrictByteString bs =
  unsafePerformIO $ BS.unsafeUseAsCStringLen bs $ uncurry utf8Decode

toStrictByteString :: JSString -> BS.ByteString
toStrictByteString str = case jsStringLength str * 3 of
  0 -> BS.empty
  max_len ->
    BS.unsafeCreateUptoN max_len $ \ptr -> utf8EncodeInto str ptr max_len

foreign import javascript unsafe
  "$1.length"
  jsStringLength :: JSString -> Int

foreign import javascript unsafe
  "(new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written"
  utf8EncodeInto :: JSString -> Ptr a -> Int -> IO Int

foreign import javascript unsafe
  "(new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  utf8Decode :: Ptr a -> Int -> IO JSString

fromStrictText :: T.Text -> JSString
fromStrictText t = T.unsafeUse t utf8Decode

toStrictText :: JSString -> T.Text
toStrictText str = T.unsafeCreateUpToN max_len $
  \ptr -> utf8EncodeInto str ptr max_len
  where
    max_len = jsStringLength str * 3
