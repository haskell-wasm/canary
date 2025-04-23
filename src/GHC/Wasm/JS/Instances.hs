{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Wasm.JS.Instances () where

import Control.Exception
import Data.Aeson qualified as A
import Data.Coerce
import GHC.Wasm.JS.Free
import GHC.Wasm.JS.JSON qualified as JSON
import GHC.Wasm.Prim
import System.IO.Unsafe

instance {-# OVERLAPPABLE #-} (Coercible JSVal v) => Show v where
  showsPrec p v r = unsafePerformIO $ do
    jstr <- jsShow $ coerce v
    str <- evaluate $ fromJSString jstr
    free jstr
    pure $ showsPrec p str r

foreign import javascript unsafe
  "`${$1}`"
  jsShow :: JSVal -> IO JSString

instance {-# OVERLAPPABLE #-} (Coercible JSVal v) => A.FromJSON v where
  parseJSON v = pure $ coerce $ JSON.toJSVal v

instance {-# OVERLAPPABLE #-} (Coercible JSVal v) => A.ToJSON v where
  toJSON v = JSON.fromJSVal $ coerce v
