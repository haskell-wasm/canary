module GHC.Wasm.JS.Free (
  free,
) where

import Data.Coerce
import GHC.Wasm.Prim

free :: Coercible JSVal v => v -> IO ()
free = coerce freeJSVal
