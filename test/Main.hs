{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main,
  )
where

import Control.Concurrent.Async.Extra
import Control.Exception
import Data.Aeson (Value)
import Data.Coerce
import Data.Stream.Monadic qualified as VS
import Data.Stream.Monadic.Extra qualified as VS
import Data.Text.Encoding qualified as T
import Data.Text.Extra qualified as T
import Data.Vector.Strict qualified as V
import GHC.Wasm.JS.Array qualified as Array
import GHC.Wasm.JS.AsyncIterator qualified as AsyncIterator
import GHC.Wasm.JS.ESM
import GHC.Wasm.JS.Free
import GHC.Wasm.JS.Instances ()
import GHC.Wasm.JS.JSON qualified as JSON
import GHC.Wasm.JS.Promise qualified as Promise
import GHC.Wasm.JS.ReadableStream qualified as ReadableStream
import GHC.Wasm.JS.RuntimeFFI
import GHC.Wasm.JS.String qualified as String
import GHC.Wasm.JS.Uint8Array qualified as Uint8Array
import GHC.Wasm.Prim
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      "GHC.Wasm.JS tests"
      [ testGroup
          "QuickCheck property tests"
          [ testProperty "unsafe ByteString <-> Text" $ \bs ->
              bs == T.unsafeToStrictByteString (T.unsafeFromStrictByteString bs),
            testProperty "unsafe Text <-> ByteString" $
              \txt -> txt == T.unsafeFromStrictByteString (T.encodeUtf8 txt),
            testProperty "string <-> JSString" $
              \s -> s == fromJSString (toJSString s),
            testProperty "Uint8Array <-> ByteString" $ \buf ->
              buf
                == Uint8Array.toStrictByteString (Uint8Array.fromStrictByteString buf),
            testProperty "Uint8Array <-> Text" $
              \txt -> txt == Uint8Array.toStrictText (Uint8Array.fromStrictText txt),
            testProperty "string <-> ByteString" $ \(T.encodeUtf8 -> bs) ->
              bs == String.toStrictByteString (String.fromStrictByteString bs),
            testProperty "string <-> Text" $
              \txt -> txt == String.toStrictText (String.fromStrictText txt),
            testProperty "Array[JSON] <-> Vector Value" $ \(vs :: V.Vector Value) ->
              vs
                == V.map
                  JSON.fromJSVal
                  (Array.toJSVals (Array.fromJSVals (V.map JSON.toJSVal vs))),
            testProperty "Promise" $ \(v :: Value) -> ioProperty $ do
              (p, res, _rej) <- Promise.newJSPromise
              (fromJSSyncCallback1 res $ JSON.toJSVal v)
                `concurrentlyR` ( do
                                    let v' = JSON.fromJSVal $ Promise.await p
                                    evaluate $ v == v'
                                ),
            testProperty "AsyncIterator/ReadableStream LazyByteString" $ \lbs ->
              ioProperty $ do
                (s, c) <- ReadableStream.newJSReadableStream ReadableStream.Bytes
                ( do
                    ReadableStream.controllerEnqueueLazyByteString c lbs
                    ReadableStream.controllerClose c
                  )
                  `concurrentlyR` ( do
                                      i <- AsyncIterator.fromJSAsyncIterable $ coerce s
                                      lbs' <- AsyncIterator.toLazyByteString i
                                      evaluate $ lbs == lbs'
                                  ),
            testProperty "AsyncIterator/ReadableStream Vector Value" $
              \(vs :: V.Vector Value) -> ioProperty $ do
                (s, c) <- ReadableStream.newJSReadableStream ReadableStream.Default
                ( do
                    V.forM_ vs $
                      \v -> ReadableStream.controllerEnqueue c $ JSON.toJSVal v
                    ReadableStream.controllerClose c
                  )
                  `concurrentlyR` ( do
                                      i <- AsyncIterator.fromJSAsyncIterable $ coerce s
                                      vs' <-
                                        VS.toVector $
                                          VS.map JSON.fromJSVal $
                                            AsyncIterator.toStream i
                                      evaluate $ vs == vs'
                                  ),
            withResource
              ( do
                  yaml <- jsImport $ jsrESM "@std/yaml"
                  evaluate $
                    callJS "$1.parse(JSON.stringify([$2, $3]))" $
                      unJSModule
                        yaml
              )
              (\_ -> pure ())
              $ \m ->
                testProperty "Any valid JSON is valid YAML" $
                  \(v1 :: Value) (v2 :: Value) -> ioProperty $ do
                    f <- m
                    evaluate $ [v1, v2] == JSON.fromJSVal (f v1 $ JSON.toJSVal v2)
          ],
        testGroup
          "ESM import tests"
          [ testCase "@babel/traverse" $ do
              m <- jsImport $ npmESM "@babel/traverse" ""
              free m,
            testCase "ollama@0.5/browser" $ do
              m <- jsImport $ npmESM "ollama@0.5" "/browser"
              free m,
            testCase "pixi.js@8" $ do
              m <- jsImport $ npmESM "pixi.js@8" ""
              free m,
            testCase "webtorrent" $ do
              m <- jsImport $ npmFile "webtorrent" "dist/webtorrent.min.js"
              free m,
            testCase "xterm.css" $ do
              e <- cssImport $ npmFile "@xterm/xterm" "css/xterm.min.css"
              elementRemove e,
            testCase "@std/async" $ do
              m <- jsImport $ jsrESM "@std/async"
              free m,
            testCase "@std/streams@1" $ do
              m <- jsImport $ jsrESM "@std/streams@1"
              free m
          ]
      ]

instance Arbitrary1 V.Vector where
  liftArbitrary g = sized $ \n -> do
    k <- chooseInt (0, n)
    V.replicateM k g

  liftShrink shr = fmap V.fromList . liftShrink shr . V.toList

instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = arbitrary1

  shrink = shrink1
