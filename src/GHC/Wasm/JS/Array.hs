module GHC.Wasm.JS.Array (
  JSArray (..),
  fromJSVals,
  toJSVals,
) where

import qualified Data.Vector.Generic as V
import GHC.Wasm.Prim
import System.IO.Unsafe

newtype JSArray = JSArray { unJSArray :: JSVal }

fromJSVals :: (V.Vector v JSVal) => v JSVal -> JSArray
fromJSVals vs = unsafePerformIO $ do
  arr <- arrayNew
  V.forM_ vs $ arrayPush arr
  pure arr

toJSVals :: (V.Vector v JSVal) => JSArray -> v JSVal
toJSVals arr = V.generate (arrayLength arr) (arrayIndex arr)

foreign import javascript unsafe
  "[]"
  arrayNew :: IO JSArray

foreign import javascript unsafe
  "$1.push($2)"
  arrayPush :: JSArray -> JSVal -> IO ()

foreign import javascript unsafe
  "$1.length"
  arrayLength :: JSArray -> Int

foreign import javascript unsafe
  "$1[$2]"
  arrayIndex :: JSArray -> Int -> JSVal
