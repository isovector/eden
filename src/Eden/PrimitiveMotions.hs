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
    inspect bLines >>= guard . not . Z.beginp
    arrests cursorY (max 0 . subtract 1)
    arrests bLines Z.left

down :: Motion
down = do
    arrests cursorY (+ 1)
    arrests bLines Z.right
    inspect bLines >>= guard . not . Z.endp

jumpStart :: Motion
jumpStart = arrest cursorX 0

jumpEnd :: Motion
jumpEnd = Y.length <$> inspect bCurLine >>= arrest cursorX

prevChar :: Motion
prevChar = do
    arrests cursorX (subtract 1)
    x <- inspect cursorX
    when (x < 0) $ do
        z <- inspect bLines
        up
        when (not $ Z.beginp z) jumpEnd

nextChar :: Motion
nextChar = do
    len   <- Y.length <$> inspect bCurLine
    arrests cursorX (+ 1)
    x <- inspect cursorX
    when (x >= len) $ do
        down
        jumpStart

dir :: a -> a -> Direction -> a
dir f _ Forwards  = f
dir _ b Backwards = b

moveChar :: Direction -> Motion
moveChar = dir nextChar prevChar

moveLine :: Direction -> Motion
moveLine = dir down up

sanitizeCursor :: Motion
sanitizeCursor = do
    len <- Y.length <$> inspect bCurLine
    arrests cursorX (max 0 . min (len - 1))
