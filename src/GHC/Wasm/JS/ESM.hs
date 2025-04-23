{-# LANGUAGE OverloadedStrings #-}

module GHC.Wasm.JS.ESM
  ( PkgSpec (..),
    JSModule (..),
    jsImport,
    JSElement (..),
    cssImport,
    elementRemove,
    npmESM,
    npmFile,
    jsrESM,
  )
where

import Control.Exception
import Data.String
import qualified Data.Text as T
import GHC.Wasm.JS.Free
import qualified GHC.Wasm.JS.String as String
import GHC.Wasm.Prim

newtype PkgSpec = PkgSpec { unPkgSpec :: T.Text }
  deriving newtype (IsString)

newtype JSModule = JSModule { unJSModule :: JSVal }

jsImport :: T.Text -> IO JSModule
jsImport url = do
  let url' = String.fromStrictText url
  m <- jsLazyImport url'
  free url'
  evaluate m

newtype JSElement = JSElement { unJSElement :: JSVal }

cssImport :: T.Text -> IO JSElement
cssImport url = do
  let url' = String.fromStrictText url
  n <- cssLazyImport url'
  free url'
  evaluate n

foreign import javascript unsafe
  "$1.remove()"
  elementRemove :: JSElement -> IO ()

npmESM :: PkgSpec -> T.Text -> T.Text
npmESM pkg rest = "https://cdn.jsdelivr.net/npm/" <> unPkgSpec pkg <> rest <> "/+esm"

npmFile :: PkgSpec -> T.Text -> T.Text
npmFile pkg f =
  "https://cdn.jsdelivr.net/npm/" <> unPkgSpec pkg <> "/" <> f

jsrESM :: PkgSpec -> T.Text
jsrESM pkg = "https://esm.sh/jsr/" <> unPkgSpec pkg

foreign import javascript safe
  "import($1)"
  jsLazyImport :: JSString -> IO JSModule

foreign import javascript safe
  "new Promise((res, rej) => { const l = document.createElement('link'); l.rel = 'stylesheet'; l.href = $1; l.addEventListener('load', () => res(l)); l.addEventListener('error', rej); document.head.append(l); })"
  cssLazyImport :: JSString -> IO JSElement
