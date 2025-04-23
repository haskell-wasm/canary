{-# LANGUAGE UndecidableInstances #-}

module GHC.Wasm.JS.RuntimeFFI (
  JSCallback (..),
  fromJSSyncCallback1,
  callJS,
) where

import Control.Exception
import qualified Data.Aeson as A
import Data.String
import qualified Data.Vector.Strict as V
import GHC.Wasm.JS.Array as Array
import GHC.Wasm.JS.Free
import qualified GHC.Wasm.JS.JSON as JSON
import GHC.Wasm.Prim
import System.IO.Unsafe

newtype JSCallback = JSCallback { unJSCallback :: JSVal }

foreign import javascript unsafe
  "dynamic"
  fromJSSyncCallback1 :: JSCallback -> JSVal -> IO ()

instance IsString JSCallback where
  fromString body = unsafePerformIO $ do
    let body' = toJSString body
    f <- jsNewCallback body'
    free body'
    pure f

foreign import javascript unsafe
  "const mk = (b) => new ((async () => {}).constructor)('$1', '$2', '$3', '$4', '$5', '$6', '$7', '$8', b); try { return mk(`return (${$1});`); } catch { return mk($1); }"
  jsNewCallback :: JSString -> IO JSCallback

class ToJS a where
  toJS :: a -> JSVal

instance ToJS JSVal where
  toJS = id

instance {-# OVERLAPPABLE #-} (A.ToJSON a) => ToJS a where
  toJS = JSON.toJSVal

class FromJS a where
  fromJS :: JSVal -> a

instance FromJS JSVal where
  fromJS = id

instance {-# OVERLAPPABLE #-} (A.FromJSON a) => FromJS a where
  fromJS = JSON.fromJSVal

strictCallJS :: JSCallback -> V.Vector JSVal -> IO JSVal
strictCallJS f xs = do
  let xs' = Array.fromJSVals xs
  r <- actuallyCallJS f xs'
  free xs'
  evaluate r

foreign import javascript safe
  "const r = await $1(...$2); return typeof r === 'undefined' ? null : r;"
  actuallyCallJS :: JSCallback -> JSArray -> IO JSVal

class CallJS r where
  accCallJS :: JSCallback -> [JSVal] -> r

instance (FromJS r) => CallJS (IO r) where
  accCallJS f xs = fmap fromJS $ strictCallJS f $ V.fromList $ reverse xs

instance {-# OVERLAPPABLE #-} (FromJS r) => CallJS r where
  accCallJS f xs =
    unsafePerformIO $ fmap fromJS $ strictCallJS f $ V.fromList $ reverse xs

instance (ToJS a, CallJS r) => CallJS (a -> r) where
  accCallJS f xs x = accCallJS f (toJS x : xs)

callJS :: CallJS r => JSCallback -> r
callJS f = accCallJS f []
