{-# LANGUAGE LambdaCase #-}

module Eden.Motions
    ( skipSpaces
    , word
    , toChar
    , findChar
    , charwiseMotions
    , linewiseMotions
    , motions
    , module Eden.PrimitiveMotions
    ) where

import Eden.Marks
import Eden.PrimitiveMotions
import Eden.Types
import Eden.Utils

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad
import Control.Monad.Loops (untilM_)
import Data.Char
import Data.Map (Map)

import qualified Data.IntMap as I
import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import qualified Yi.Rope as Y

skipSpaces :: Motion
skipSpaces = do
    cur <- cursorChar
    when (isSpace cur) $
        nextChar `untilM_` liftM (not . isSpace) cursorChar

word :: Motion
word = do
    lineNum <- inspect cursorY
    skipChars <- liftM2 (||) isPunctuation isSymbol <$> cursorChar
    nextChar `untilM_` wordBoundary lineNum skipChars
    skipSpaces
  where
    wordBoundary lineNum skip = do
        cur     <- cursorChar
        curLine <- inspect cursorY
        return . (||) (curLine /= lineNum) . not
               $ if skip
                    then liftM2 (||) isPunctuation isSymbol $ cur
                    else isAlphaNum cur

toChar :: Repeatable Buffer ()
toChar = do
    char <- again . liftIO $ getChar
    lift $ do
        nextChar `untilM_` liftM (== char) cursorChar
        prevChar

findChar :: Repeatable Buffer ()
findChar = do
    char <- again . liftIO $ getChar
    lift $ do
        cur <- cursorChar
        when (cur /= char) $ do
            nextChar `untilM_` liftM (== char) cursorChar

charwiseMotions :: Map String Motion
charwiseMotions = M.fromList
    [ ("h", proclaims cursorX (subtract 1))
    , ("l", proclaims cursorX (+ 1))
    , ("w", word)
    , ("0", jumpStart)
    , ("$", jumpEnd)
    ]

linewiseMotions :: Map String Motion
linewiseMotions = M.fromList
    [ ("j", down)
    , ("k", up)
    ]

motions :: Map String Motion
motions = charwiseMotions `M.union` linewiseMotions
