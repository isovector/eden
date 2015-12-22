module Main where

import Eden.Interface
import Eden.Modes
import Eden.Types
import Eden.Utils

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.Maybe (fromJust)

harness :: Eden World ()
harness = do
    withNextBuffer $ loadFile "eden.cabal"
    forever prompt

main :: IO ()
main = do
    s <- snd . fromJust <$> runJurisdictionT harness emptyWorld
    seq s $ return ()

