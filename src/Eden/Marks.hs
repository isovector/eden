{-# LANGUAGE LambdaCase #-}

module Eden.Marks where

import Eden.Motions
import Eden.Types
import Eden.Utils

import Control.Applicative ((<$>))

sign :: Int -> Int
sign x |  x >  0 =  1
       |  x == 0 =  0
       |  x <  0 = -1

jumpToLine :: Int -> Motion
jumpToLine y = do
    (_, cy) <- inspect bCursor
    let dy = y - cy
        n  = take (abs dy) [0..]
    case sign dy of
       -1 -> mapM_ (const up) n
       0  -> return ()
       1  -> mapM_ (const down) n

jumpToMark :: Mark -> Motion
jumpToMark (Mark mx my) = do
    jumpToLine my
    proclaim cursorX mx
    return ()

getMark :: Eden Buffer Mark
getMark = uncurry Mark <$> inspect bCursor

charwiseTowards :: Eden Buffer () -> Mark -> Eden Buffer ()
charwiseTowards what there = do
    dir >>= \case
        LT -> do what
                 prevChar
                 charwiseTowards what there
        EQ -> return ()
        GT -> do what
                 nextChar
                 charwiseTowards what there
  where
    dir = do
        here <- getMark
        return $ compare there here


linewiseTowards :: Eden Buffer () -> Mark -> Eden Buffer ()
linewiseTowards what there = do
    dir >>= \case
        LT -> do what
                 up
                 linewiseTowards what there
        EQ -> what
        GT -> do what
                 down
                 linewiseTowards what there
  where
    dir = do
        here <- getMark
        return $ compare (view markY there) (view markY here)

