module Eden.Utils where

import Eden.Types

import Control.Lens
import Data.List.Zipper (Zipper)
import qualified Yi.Rope as Y
import qualified Data.List.Zipper as Z


withNextBuffer :: Eden (Maybe Buffer) a -> Eden World a
withNextBuffer n = do
    next   <- inquire wNextBuffer
    result <- restrict (wBuffers . at next) n
    proclaims wNextBuffer (+1)
    return result

withCurBuffer :: Eden Buffer () -> Eden World ()
withCurBuffer n = do
    current <- inquire wCurBuffer
    restrictInto (wBuffers . at current) n

curBuffer :: Eden a (Maybe Buffer)
curBuffer = do
    current <- inquire wCurBuffer
    inquire (wBuffers . at current)

withLines :: ((Int, Y.YiString) -> Y.YiString) -> Y.YiString -> Y.YiString
withLines f = Y.unlines . map f . zip [0..] . Y.lines

insert :: Int -> Y.YiString -> Zipper Y.YiString -> Zipper Y.YiString
insert x what z = let line = Z.cursor z
                   in Z.replace (yInsert x what line) z

yInsert :: Int -> Y.YiString -> Y.YiString -> Y.YiString
yInsert x what line = let (left,right) = Y.splitAt x line
                       in Y.concat [left, what, right]
