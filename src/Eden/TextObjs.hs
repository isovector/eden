module Eden.TextObjs where

import Eden.Marks
import Eden.Motions
import Eden.Types
import Eden.Utils

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Data.IORef
import Data.Map (Map)

import qualified Data.Map as M
import qualified Yi.Rope as Y


textobjs :: Map String (Eden Buffer TextObj)
textobjs = M.fromList $
    [
    -- ("aw", aWord)
    ] ++ map (second liftCharwise) (M.toList charwiseMotions)
      ++ map (second liftLinewise) (M.toList linewiseMotions)

emptyObj :: TextObj
emptyObj = let emptyMark = Mark 0 0
            in TextObj Charwise emptyMark emptyMark

liftCharwise :: Motion -> Eden Buffer TextObj
liftCharwise m = do
    here <- getMark
    m
    there <- getMark
    jumpToMark here
    return $ TextObj Charwise (min here there) (max here there)

liftLinewise :: Motion -> Eden Buffer TextObj
liftLinewise m = do
    here <- getMark
    m
    there <- getMark

    let (first, last) = (min here there, max here there)
    jumpToMark last
    lineLen <- Y.length <$> inspect bCurLine
    jumpToMark here

    -- Extend x positions to be the start and end of lines
    let first' = set markX 0 first
        last'  = set markX lineLen last
    return $ TextObj Linewise first' last'


getTextObj :: Repeatable Buffer (Maybe (Eden Buffer TextObj))
getTextObj = again $ do
    remaining <- liftIO . newIORef $ M.toList textobjs
    go remaining
  where
    go remaining = do
        c <- liftIO getChar
        liftIO . modifyIORef remaining $ map (first tail)
                                       . filter ((== c) . head . fst)
        results <- liftIO $ readIORef remaining
        case length results of
            0 -> return Nothing
            1 -> return . Just . snd $ head results
            _ -> go remaining

