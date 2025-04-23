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
import GHC.Wasm.JS.URL (JSURL)
import qualified GHC.Wasm.JS.URL as URL
import GHC.Wasm.Prim

newtype PkgSpec = PkgSpec { unPkgSpec :: T.Text }
  deriving newtype (IsString)

newtype JSModule = JSModule { unJSModule :: JSVal }

jsImport :: JSURL -> IO JSModule
jsImport url = do
  m <- jsLazyImport url
  evaluate m

newtype JSElement = JSElement { unJSElement :: JSVal }

cssImport :: JSURL -> IO JSElement
cssImport url = do
  n <- cssLazyImport url
  evaluate n

foreign import javascript unsafe
  "$1.remove()"
  elementRemove :: JSElement -> IO ()

npmESM :: PkgSpec -> T.Text -> JSURL
npmESM pkg rest = URL.fromStrictText $ "https://cdn.jsdelivr.net/npm/" <> unPkgSpec pkg <> rest <> "/+esm"

npmFile :: PkgSpec -> T.Text -> JSURL
npmFile pkg f = URL.fromStrictText $
  "https://cdn.jsdelivr.net/npm/" <> unPkgSpec pkg <> "/" <> f

jsrESM :: PkgSpec -> JSURL
jsrESM pkg = URL.fromStrictText $ "https://esm.sh/jsr/" <> unPkgSpec pkg

foreign import javascript safe
  "import($1)"
  jsLazyImport :: JSURL -> IO JSModule

foreign import javascript safe
  "new Promise((res, rej) => { const l = document.createElement('link'); l.rel = 'stylesheet'; l.href = $1; l.addEventListener('load', () => res(l)); l.addEventListener('error', rej); document.head.append(l); })"
  cssLazyImport :: JSURL -> IO JSElement
