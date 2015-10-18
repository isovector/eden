{-# LANGUAGE JavaScriptFFI, CPP #-}

module Main (main, reverseJSRef) where

import GHCJS.Foreign
import GHCJS.Types

foreign import javascript unsafe "console.log($1);"
  consoleLog :: JSString -> IO ()

main :: IO ()
main = putStrLn "Dummy main"

reverseJSRef :: JSString -> IO ()
reverseJSRef input = consoleLog input
