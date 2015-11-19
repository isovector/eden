module Eden.Modes
    ( modes
    , insertMode
    , normalMode
    , loadFile
    , module Eden.Modes.Normal
    ) where

import Eden.Modes.Normal
import Eden.Motions
import Eden.Operators
import Eden.Types
import Eden.Utils

import Control.Monad
import Data.List (intercalate)
import Data.Map (Map)

import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import qualified Yi.Rope as Y

modes :: Map Mode (Eden World ())
modes = M.fromList
    [ (NORMAL, normalMode)
    , (INSERT, insertMode)
    ]

-- TODO(maguirea): enter breaks our cursor sync
insertMode :: Eden World ()
insertMode = do
    char <- liftIO getChar
    if char == '\x1b'
        then proclaim wMode NORMAL
        else withCurBuffer $ do
                 (x,_) <- inspect bCursor
                 proclaims bCurLine $ insert x (Y.fromString [char])
                 proclaims cursorX (+ 1)

normalMode :: Eden World ()
normalMode = do
    result <- liftIO getChar
    if result == ':'
       then do
           line <- liftIO getLine
           liftM2 (commands M.!) head tail $ words line
       else nnoremap M.! result

nnoremap :: Map Char (Eden World ())
nnoremap = M.fromList $
    [ ('O', openLine $ return ())
    , ('o', openLine down)
    , ('x', withCurBuffer delChar)
    , ('d', runOperator deleteOp =<< liftMotion word)
    , ('c', runOperator changeOp =<< liftMotion word)
    , ('i', proclaim wMode INSERT)
    , ('\x1b', proclaim wMode NORMAL)
    ] ++ map toNMap (M.toList motions)
  where
    toNMap (key, motion) = (head key, withCurBuffer motion)

commands :: Map String ([String] -> Eden World ())
commands = M.fromList
    [ ("e", asWords $ withNextBuffer . loadFile)
    ]

loadFile :: FilePath -> Eden (Maybe Buffer) ()
loadFile f = do
    result <- liftIO $ Y.readFile f
    put . Just . Buffer f (0, 0) $ case result of
        Right (text, _) -> Z.fromList $ Y.lines text
        Left _          -> error "bad file"

asWords :: (String -> b) -> ([String] -> b)
asWords f = f . intercalate " "

