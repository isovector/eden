module Main where

import Eden.Interface
import Eden.Modes
import Eden.Types
import Eden.Utils

import Control.Applicative ((<$>))
import Control.Monad (forever)

harness :: Eden World ()
harness = do
    withNextBuffer $ loadFile "eden.cabal"
    forever prompt

main :: IO ()
main = do
    s <- snd <$> runJurisdictionT harness emptyWorld
    seq s $ return ()

