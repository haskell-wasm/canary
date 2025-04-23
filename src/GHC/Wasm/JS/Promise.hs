module GHC.Wasm.JS.Promise (
  JSPromise (..),
  newJSPromise,
  await,
  awaitIO,
) where

import Control.Exception
import GHC.Wasm.JS.Free
import GHC.Wasm.JS.RuntimeFFI
import GHC.Wasm.Prim

newtype JSPromise = JSPromise { unJSPromise :: JSVal }

newtype JSPromiseWithResolvers = JSPromiseWithResolvers JSVal

newJSPromise :: IO (JSPromise, JSCallback, JSCallback)
newJSPromise = do
  r <- jsPromiseWithResolvers
  p <- jsPromise r
  res <- jsResolve r
  rej <- jsReject r
  free r
  pure (p, res, rej)

awaitIO :: JSPromise -> IO JSVal
awaitIO = evaluate . await

foreign import javascript safe
  "$1"
  await :: JSPromise -> JSVal

foreign import javascript unsafe
  "Promise.withResolvers()"
  jsPromiseWithResolvers :: IO JSPromiseWithResolvers

foreign import javascript unsafe
  "$1.promise"
  jsPromise :: JSPromiseWithResolvers -> IO JSPromise

foreign import javascript unsafe
  "$1.resolve"
  jsResolve :: JSPromiseWithResolvers -> IO JSCallback

foreign import javascript unsafe
  "$1.reject"
  jsReject :: JSPromiseWithResolvers -> IO JSCallback
