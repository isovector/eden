module Eden.PrimitiveMotions where

import Eden.Types
import Eden.Utils

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad
import Data.Map (Map)

import qualified Data.IntMap as I
import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import qualified Yi.Rope as Y


up :: Motion
up = do
    z <- inspect bLines
    when (not $ Z.beginp z) $ do
        proclaims cursorY (max 0 . subtract 1)
        proclaims (bLines) Z.left

down :: Motion
down = do
    z <- inspect bLines
    when (not $ Z.endp z) $ do
        proclaims cursorY (+ 1)
        proclaims (bLines) Z.right

jumpStart :: Motion
jumpStart = proclaim cursorX 0

jumpEnd :: Motion
jumpEnd = Y.length <$> inspect bCurLine >>= proclaim cursorX

prevChar :: Motion
prevChar = do
    proclaims cursorX (subtract 1)
    x <- inspect cursorX
    when (x < 0) $ do
        z <- inspect bLines
        up
        when (not $ Z.beginp z) jumpEnd

nextChar :: Motion
nextChar = do
    len   <- Y.length <$> inspect bCurLine
    proclaims cursorX (+ 1)
    x <- inspect cursorX
    when (x >= len) $ do
        down
        jumpStart

sanitizeCursor :: Motion
sanitizeCursor = do
    len <- Y.length <$> inspect bCurLine
    proclaims cursorX (max 0 . min (len - 1))
