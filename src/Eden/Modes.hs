module Eden.Modes where

import Eden.Utils
import Eden.Types

import Control.Lens
import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Yi.Rope as Y
import qualified Data.List.Zipper as Z


loadFile :: FilePath -> Eden (Maybe Buffer) ()
loadFile f = do
    result <- liftIO $ Y.readFile f
    put . Just . Buffer f (0, 0) $ case result of
        Right (text, _) -> Z.fromList $ Y.lines text
        Left _          -> error "bad file"

asWords :: (String -> b) -> ([String] -> b)
asWords f = f . intercalate " "

modes :: Map Mode (Eden World ())
modes = M.fromList
    [ (NORMAL, normalMode)
    , (INSERT, insertMode)
    ]

up :: Eden Buffer ()
up = do
    z <- gets $ view bLines
    if not $ Z.beginp z
        then do
             proclaims (bCursor . _2) (subtract 1)
             proclaims (bLines) Z.left
        else return ()

down :: Eden Buffer ()
down = do
    z <- gets $ view bLines
    if not $ Z.endp z
        then do
             proclaims (bCursor . _2) (+ 1)
             proclaims (bLines) Z.right
        else return ()

normalMode :: Eden World ()
normalMode = do
    result <- liftIO getChar
    if result == ':'
       then do
           line <- liftIO getLine
           liftM2 (commands M.!) head tail $ words line
       else nnoremap M.! result

insertMode :: Eden World ()
insertMode = do
    char <- liftIO getChar
    if char == '\x1b'
        then proclaim wMode NORMAL
        else withCurBuffer $ do
                 (x,_) <- gets $ view bCursor
                 proclaims bCurLine $ insert x (Y.fromString [char])
                 proclaims (bCursor . _1) (+ 1)

nnoremap :: Map Char (Eden World ())
nnoremap = M.fromList
    [ ('j', withCurBuffer down)
    , ('k', withCurBuffer up)
    , ('h', withCurBuffer $ proclaims (bCursor . _1) (subtract 1))
    , ('l', withCurBuffer $ proclaims (bCursor . _1) (+ 1))
    , ('i', proclaim wMode INSERT)
    , ('\x1b', proclaim wMode NORMAL)
    ]

commands :: Map String ([String] -> Eden World ())
commands = M.fromList
    [ ("e", asWords $ withNextBuffer . loadFile)
    ]
