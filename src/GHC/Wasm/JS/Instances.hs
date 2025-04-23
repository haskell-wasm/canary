{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}

module GHC.Wasm.JS.Instances {-# WARNING "FOR DEBUGGING ONLY" #-} () where

import Control.DeepSeq
import Control.Exception
import Data.Aeson qualified as A
import Data.Coerce
import Data.String
import Data.Text qualified as T
import GHC.Wasm.JS.Free
import GHC.Wasm.JS.JSON qualified as JSON
import GHC.Wasm.JS.String qualified as String
import GHC.Wasm.Prim
import System.IO.Unsafe

instance {-# OVERLAPPABLE #-} (Coercible JSVal v) => Eq v where
  (==) = coerce jsStrictEq

foreign import javascript unsafe
  "$1 === $2"
  jsStrictEq :: JSVal -> JSVal -> Bool

instance {-# OVERLAPPABLE #-} (Coercible JSVal v) => IsString v where
  fromString = coerce . String.fromStrictText . T.pack

instance {-# OVERLAPPABLE #-} (Coercible JSVal v) => NFData v where
  rnf v = seq v ()

instance {-# OVERLAPPABLE #-} (Coercible JSVal v) => Show v where
  showsPrec p v r = unsafePerformIO $ do
    jstr <- jsShow $ coerce v
    txt <- evaluate $ String.toStrictText jstr
    free jstr
    pure $ showsPrec p txt r

foreign import javascript unsafe
  "`${$1}`"
  jsShow :: JSVal -> IO JSString

instance {-# OVERLAPPABLE #-} (Coercible JSVal v) => A.FromJSON v where
  parseJSON v = pure $ coerce $ JSON.toJSVal v

instance {-# OVERLAPPABLE #-} (Coercible JSVal v) => A.ToJSON v where
  toJSON v = JSON.fromJSVal $ coerce v
