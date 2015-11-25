module Eden.Utils where

import Eden.Types

import Control.Lens
import Data.List.Zipper (Zipper)

import qualified Data.List.Zipper as Z
import qualified Yi.Rope          as Y

withNextBuffer :: Eden (Maybe Buffer) a -> Eden World a
withNextBuffer n = do
    next   <- inquire wNextBuffer
    result <- restrict (wBuffers . at next) n
    proclaims wNextBuffer (+1)
    return result

maybeWithCurBuffer :: a -> Eden Buffer a -> Eden World a
maybeWithCurBuffer d n = do
    current <- inquire wCurBuffer
    restrictInto (wBuffers . at current) d n

withCurBuffer :: Eden Buffer () -> Eden World ()
withCurBuffer = maybeWithCurBuffer ()

curBuffer :: Eden a (Maybe Buffer)
curBuffer = do
    current <- inquire wCurBuffer
    inquire (wBuffers . at current)

cursorX :: RLens Int Buffer
cursorX = bCursor . _1

cursorY :: RLens Int Buffer
cursorY = bCursor . _2

cursorChar :: Eden Buffer Char
cursorChar = do
    x <- inspect cursorX
    restrict bCurLine $ do
        (_, right) <- gets $ Y.splitAt x
        case Y.head right of
            Just x ->  return x
            Nothing -> return '\0'

insert :: Int -> Y.YiString -> Y.YiString -> Y.YiString
insert x what line = let (left,right) = Y.splitAt x line
                      in Y.concat [left, what, right]

lineBreak :: Int -> Zipper Y.YiString -> Zipper Y.YiString
lineBreak x z = let (left, right) = Y.splitAt x $ Z.cursor z
                    sameLine = Z.replace left z
                    nextLine = Z.right sameLine
                 in Z.insert right nextLine

delete :: Int -> Int -> Y.YiString -> Y.YiString
delete x width line = let (left, right) = Y.splitAt x line
                       in Y.concat [left, Y.drop width right]
