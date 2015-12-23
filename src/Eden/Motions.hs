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

import Eden.PrimitiveMotions
import Eden.Types
import Eden.Utils

import Control.Monad
import Control.Monad.Loops (untilM_)
import Data.Char
import Data.Map (Map)

import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import qualified Yi.Rope as Y


onLine :: Motion -> Motion
onLine m = do
    here <- inspect cursorY
    try $ do
        m
        inspect cursorY >>= guard . (here ==)

before :: Motion -> Motion
before m = m >> prevChar

-- TODO(sandy): need to make this reversible
findNextChar :: (Char -> Bool) -> Direction -> Motion
findNextChar p d = moveChar d `untilM_` liftM p cursorChar

-- TODO(sandy): need to make this reversible
findNextLine :: (Y.YiString -> Bool) -> Direction -> Motion
findNextLine p d = moveLine d `untilM_` liftM p $ Z.cursor <$> inspect bLines

ifIs :: (Char -> Bool) -> Motion -> Motion
ifIs p m = do
    x <- cursorChar
    when (p x) m

skipSpaces :: Motion
skipSpaces = ifIs isSpace $ findNextChar (not . isSpace) Forwards

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

-- TODO(sandy): this should land on the last character of the last line
paragraph :: Direction -> Motion
paragraph = findNextLine Y.null

toChar :: Direction -> Repeatable Buffer ()
toChar d = do
    char <- again . liftIO $ getChar
    lift . onLine . before $ findNextChar (== char) d

findChar :: Direction -> Repeatable Buffer ()
findChar d = do
    char <- again . liftIO $ getChar
    lift . onLine $ findNextChar (== char) d

charwiseMotions :: Map String Motion
charwiseMotions = M.fromList
    [ ("h", proclaims cursorX (subtract 1))
    , ("l", proclaims cursorX (+ 1))
    , ("w", word)
    , ("0", jumpStart)
    , ("$", jumpEnd)
    , ("{", paragraph Backwards)
    , ("}", paragraph Forwards)
    ]

linewiseMotions :: Map String Motion
linewiseMotions = M.fromList
    [ ("j", down)
    , ("k", up)
    ]

motions :: Map String Motion
motions = charwiseMotions `M.union` linewiseMotions
