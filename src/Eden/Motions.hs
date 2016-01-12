{-# LANGUAGE LambdaCase #-}

module Eden.Motions
    ( skipSpaces
    , word
    , toChar
    , findChar
    , snipe
    , charwiseMotions
    , linewiseMotions
    , motions
    , repeatMotion
    , module Eden.PrimitiveMotions
    ) where

import Eden.PrimitiveMotions
import Eden.Types
import Eden.Utils

import Control.Monad
import Control.Monad.Loops (untilM_)
import Control.Monad.Reader
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

before :: Direction -> Motion -> Motion
before d m = m >> moveChar (otherDir d)

findNext :: (Y.YiString -> Bool) -> Direction -> Motion
findNext p d = do
    untilM_ (moveChar d) $ do
        x <- inspect cursorX
        liftM (p . view (stringRemainder x))
            $ Z.cursor <$> inspect bLines

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

reversible :: Direction
           -> (Direction -> Motion)
           -> ReaderT Direction (Eden Buffer) ()
reversible d m = ask >>= \case
    Forwards  -> lift $ m d
    Backwards -> lift . m $ otherDir d

toChar :: Direction -> Again (ReaderT Direction (Eden Buffer)) ()
toChar d = do
    char <- again . liftIO $ getChar
    lift . reversible d $
        \d' -> onLine . before d' $ findNextChar (== char) d'

findChar :: Direction -> Again (ReaderT Direction (Eden Buffer)) ()
findChar d = do
    char <- again . liftIO $ getChar
    lift . reversible d $ onLine . findNextChar (== char)

snipe :: Direction -> Again (ReaderT Direction (Eden Buffer)) ()
snipe d = do
    (c1, c2) <- again . liftIO $ liftM2 (,) getChar getChar
    let str = Y.cons c1 $ Y.singleton c2
    lift . reversible d $ try . findNext ((== str) . (Y.take 2))

repeatMotion :: Direction -> Eden Buffer ()
repeatMotion = (inquire wRepMotion >>=) . flip runReaderT

charwiseMotions :: Map String Motion
charwiseMotions = M.fromList
    [ ("h", arrests cursorX (subtract 1))
    , ("l", arrests cursorX (+ 1))
    , ("w", word)
    , ("0", jumpStart)
    , ("$", jumpEnd)
    , ("{", paragraph Backwards)
    , ("}", paragraph Forwards)
    , ("t", repeatableMotion $ toChar Forwards)
    , ("f", repeatableMotion $ findChar Forwards)
    , ("T", repeatableMotion $ toChar Backwards)
    , ("F", repeatableMotion $ findChar Backwards)
    , ("s", repeatableMotion $ snipe Forwards)
    , ("S", repeatableMotion $ snipe Backwards)
    , (";", repeatMotion Forwards)
    , (",", repeatMotion Backwards)
    ]

linewiseMotions :: Map String Motion
linewiseMotions = M.fromList
    [ ("j", down)
    , ("k", up)
    ]

motions :: Map String Motion
motions = charwiseMotions `M.union` linewiseMotions
