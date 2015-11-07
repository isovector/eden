{-# LANGUAGE LambdaCase #-}

module Main where

import Eden.Types
import Eden.Interface

import Control.Applicative ((<$>))
import Control.Monad
import Control.Lens
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.Map (Map)
import qualified Data.Map as M
import qualified Yi.Rope as Y

loadFile :: FilePath -> Eden (Maybe Buffer) ()
loadFile f = do
    result <- liftIO $ Y.readFile f
    put . Just . Buffer f (0, 0) $ case result of
        Right (text, _) -> text
        Left _          -> error "bad file"

main :: IO ()
main = do
    s <- snd <$> runJurisdictionT (forever prompt) emptyWorld
    seq s $ return ()
