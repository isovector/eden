module Eden.Utils where

import Eden.Types
import Control.Lens
import qualified Yi.Rope as Y

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

insert :: Pos -> Y.YiString -> Y.YiString -> Y.YiString
insert (cx, cy) what s = withLines go s
    where go (y, line) = if y == cy
                             then yInsert cx what line
                             else line

yInsert :: Int -> Y.YiString -> Y.YiString -> Y.YiString
yInsert x what line = let (left,right) = Y.splitAt x line
                       in Y.concat [left, what, right]
