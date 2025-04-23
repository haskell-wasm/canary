module GHC.Wasm.JS.JSON (
  fromJSVal,
  toJSVal,
) where

import Control.Exception
import qualified Data.Aeson as A
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import Foreign.Ptr
import GHC.Wasm.JS.Free
import GHC.Wasm.JS.String
import GHC.Wasm.Prim
import System.IO.Unsafe

fromJSVal :: A.FromJSON a => JSVal -> a
fromJSVal v = unsafePerformIO $ do
  str <- jsonStringify v
  a <- evaluate =<< A.throwDecodeStrictText (toStrictText str)
  free str
  pure a

toJSVal :: A.ToJSON a => a -> JSVal
toJSVal a =
  unsafePerformIO $
    BS.unsafeUseAsCStringLen (LBS.toStrict $ A.encode a) $
      uncurry jsonParse

foreign import javascript unsafe
  "JSON.stringify($1)"
  jsonStringify :: JSVal -> IO JSString

foreign import javascript unsafe
  "JSON.parse((new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2)))"
  jsonParse :: Ptr a -> Int -> IO JSVal
