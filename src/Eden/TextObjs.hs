module Eden.TextObjs where

import Eden.Marks
import Eden.Motions
import Eden.Types
import Eden.Utils

import Control.Arrow (first, second)
import Data.IORef
import Data.Map (Map)

import qualified Data.Map as M


textobjs :: Map String (Eden World TextObj)
textobjs = M.fromList $
    [ 
    -- ("aw", aWord)
    ] ++ map (second liftMotion) (M.toList motions)

liftMotion :: Motion -> Eden World TextObj
liftMotion m = do
    let emptyMark = Mark 0 0
        empty = (emptyMark, emptyMark)
    maybeWithCurBuffer empty $ do
        here <- getMark
        m
        there <- getMark
        jumpToMark here
        return (min here there, max here there)

getTextObj :: Eden World (Maybe (Eden World TextObj))
getTextObj = do
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
