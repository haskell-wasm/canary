module GHC.Wasm.JS.URL
  ( JSURL (..),
    fromStrictText,
  )
where

import Data.String
import Data.Text qualified as T
import GHC.Wasm.JS.Free
import GHC.Wasm.JS.String qualified as String
import GHC.Wasm.Prim
import System.IO.Unsafe

newtype JSURL = JSURL { unJSURL :: JSVal }

instance IsString JSURL where
  fromString = fromStrictText . T.pack

fromStrictText :: T.Text -> JSURL
fromStrictText t = unsafePerformIO $ do
  let js = String.fromStrictText t
  r <- jsURLFromJSString js
  free js
  pure r

foreign import javascript unsafe
  "new URL($1, document.baseURI)"
  jsURLFromJSString :: JSString -> IO JSURL
