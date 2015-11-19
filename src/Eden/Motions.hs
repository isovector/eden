module Eden.Motions where

import Eden.Types
import Eden.Utils

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad
import Control.Monad.Loops (untilM_)
import Data.Char

import qualified Data.IntMap as I
import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import qualified Yi.Rope as Y


up :: Motion
up = do
    z <- inspect bLines
    if not $ Z.beginp z
        then do
             proclaims cursorY (subtract 1)
             proclaims (bLines) Z.left
        else return ()

down :: Motion
down = do
    z <- inspect bLines
    if not $ Z.endp z
        then do
             proclaims cursorY (+ 1)
             proclaims (bLines) Z.right
        else return ()

jumpStart :: Motion
jumpStart = proclaim cursorX 0

jumpEnd :: Motion
jumpEnd =   subtract 1 . Y.length
        <$> inspect bCurLine >>= proclaim cursorX

prevChar :: Motion
prevChar = do
    proclaims cursorX (subtract 1)
    x <- inspect cursorX
    if x < 0
        then do
            up
            jumpEnd
        else return ()

nextChar :: Motion
nextChar = do
    len   <- Y.length <$> inspect bCurLine
    proclaims cursorX (+ 1)
    x <- inspect cursorX
    if x >= len
        then do
            down
            jumpStart
        else return ()

skipSpaces :: Motion
skipSpaces = do
    cur <- cursorChar
    if isSpace cur
        then nextChar `untilM_` liftM (not . isSpace) cursorChar
        else return ()

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
