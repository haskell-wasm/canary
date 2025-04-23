module GHC.Wasm.JS.AsyncIterator (
  JSAsyncIterable (..),
  JSAsyncIterator (..),
  fromJSAsyncIterable,
  toStream,
  toLazyByteString,
) where

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Stream.Monadic as VS
import qualified Data.Stream.Monadic.Extra as VS
import GHC.Wasm.JS.Free
import GHC.Wasm.JS.Uint8Array
import GHC.Wasm.Prim

newtype JSAsyncIterable = JSAsyncIterable { unJSAsyncIterable :: JSVal }

newtype JSAsyncIterator = JSAsyncIterator { unJSAsyncIterator :: JSVal }

foreign import javascript unsafe
  "$1[Symbol.asyncIterator] ? $1[Symbol.asyncIterator]() : $1[Symbol.iterator]()"
  fromJSAsyncIterable :: JSAsyncIterable -> IO JSAsyncIterator

newtype JSIteratorResult = IteratorResult JSVal

toStream :: MonadIO m => JSAsyncIterator -> VS.Stream m JSVal
toStream iter = flip VS.Stream () $ \_ -> liftIO $ do
  r <- jsAsyncIteratorNext iter
  if jsIteratorResultDone r
    then do
      free r
      pure VS.Done
    else do
      v <- jsIteratorResultValue r
      free r
      pure $ VS.Yield v ()

toLazyByteString :: JSAsyncIterator -> IO LBS.ByteString
toLazyByteString =
  fmap LBS.fromChunks
    . VS.toLazyList
    . VS.mapM
      ( \v -> do
          c <- evaluate $ toStrictByteString $ JSUint8Array v
          free v
          pure c
      )
    . toStream

foreign import javascript safe
  "$1.next()"
  jsAsyncIteratorNext :: JSAsyncIterator -> IO JSIteratorResult

foreign import javascript unsafe
  "$1.done"
  jsIteratorResultDone :: JSIteratorResult -> Bool

foreign import javascript unsafe
  "$1.value"
  jsIteratorResultValue :: JSIteratorResult -> IO JSVal
