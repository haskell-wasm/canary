module GHC.Wasm.JS.ReadableStream (
  StreamType (..),
  JSReadableStream (..),
  JSReadableStreamController (..),
  newJSReadableStream,
  controllerClose,
  controllerEnqueue,
  controllerError,
  controllerEnqueueLazyByteString,
) where

import qualified Data.ByteString.Lazy as LBS
import GHC.Wasm.JS.Free
import qualified GHC.Wasm.JS.Uint8Array as Uint8Array
import GHC.Wasm.Prim

data StreamType = Bytes | Default
  deriving stock (Eq)

newtype JSReadableStream = JSReadableStream { unJSReadableStream :: JSVal }

newtype JSReadableStreamController = JSReadableStreamController { unJSReadableStreamController :: JSVal }

newtype JSReadableStreamWithController = JSReadableStreamWithController JSVal

newJSReadableStream :: StreamType -> IO (JSReadableStream, JSReadableStreamController)
newJSReadableStream ty = do
  r <- jsReadableStreamWithController $ ty == Bytes
  s <- jsStream r
  c <- jsController r
  free r
  pure (s, c)

foreign import javascript unsafe
  "$1.close()"
  controllerClose :: JSReadableStreamController -> IO ()

foreign import javascript unsafe
  "$1.enqueue($2)"
  controllerEnqueue :: JSReadableStreamController -> JSVal -> IO ()

foreign import javascript unsafe
  "$1.error($2)"
  controllerError :: JSReadableStreamController -> JSVal -> IO ()

foreign import javascript unsafe
  "let controller; const stream = new ReadableStream({start(c) { controller = c; }, type: $1 ? 'bytes' : undefined}); return { stream, controller };"
  jsReadableStreamWithController :: Bool -> IO JSReadableStreamWithController

foreign import javascript unsafe
  "$1.stream"
  jsStream :: JSReadableStreamWithController -> IO JSReadableStream

foreign import javascript unsafe
  "$1.controller"
  jsController :: JSReadableStreamWithController -> IO JSReadableStreamController

controllerEnqueueLazyByteString :: JSReadableStreamController -> LBS.ByteString -> IO ()
controllerEnqueueLazyByteString c = flip LBS.foldrChunks (pure ()) $ \buf m ->
  controllerEnqueue
    c
    (Uint8Array.unJSUint8Array $ Uint8Array.fromStrictByteString buf)
    *> m
