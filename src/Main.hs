module Main where

import Eden.Types
import Eden.Interface

import Control.Applicative ((<$>))
import Control.Monad (forever)


main :: IO ()
main = do
    s <- snd <$> runJurisdictionT (forever prompt) emptyWorld
    seq s $ return ()
