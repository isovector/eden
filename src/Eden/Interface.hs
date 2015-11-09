module Eden.Interface where

import Eden.Modes
import Eden.Types
import Eden.Utils

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Loops (untilM_)
import Data.Map (Map)
import System.Console.ANSI
import System.IO

import qualified Data.IntMap      as I
import qualified Data.List.Zipper as Z
import qualified Data.Map         as M
import qualified Yi.Rope          as Y

display :: Buffer -> IO ()
display b = do
    let clines = Z.toList $ view bLines b
        cursor = view bCursor b
        wcursor = map (inject cursor) $ zip [0..] clines
    forM_ wcursor $ putStrLn . Y.toString
  where
      inject (x,y) (cy, line) =
          if y == cy
             then let (left,right) = Y.splitAt x line
                      (char,rest) = Y.splitAt 1 right
                   in Y.concat [left, highlight char, rest]
             else line
      highlight char = Y.fromString $ concat
          [ setSGRCode [ SetColor Foreground Vivid Black
                       , SetColor Background Vivid White
                       ]
          , Y.toString char
          , setSGRCode [Reset]
          ]

prompt :: Eden World ()
prompt = do
    world  <- get
    buffer <- maybe emptyBuffer id <$> curBuffer
    mode   <- inquire wMode
    liftIO $ do
        hFlush stdout
        forM_ [1..30] . const $ putStrLn ""
        display buffer
        putStrLn ""
        putStr . show $ mode
        putStr " "
        putStr . show . I.size $ view wBuffers world
        putStr "> "
        hFlush stdout
    modes M.! mode


