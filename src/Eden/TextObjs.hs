module Eden.TextObjs where

import Eden.Motions
import Eden.Operators
import Eden.Types

import Control.Arrow (first, second)
import Data.IORef
import Data.Map (Map)

import qualified Data.Map as M


textobjs :: Map String (Eden World TextObj)
textobjs = M.fromList $
    [ 
    -- ("aw", aWord)
    ] ++ map (second liftMotion) (M.toList motions)

getTextObj :: Eden World (Maybe (Eden World TextObj))
getTextObj = do
    remaining <- liftIO . newIORef $ M.toList textobjs
    c         <- liftIO getChar
    liftIO . modifyIORef remaining $ map (first tail)
                                   . filter ((== c) . head . fst)
    results <- liftIO $ readIORef remaining
    case length results of
      0 -> return Nothing
      1 -> return . Just . snd $ head results
      -- TODO(sandy): recurse
      _ -> return Nothing
