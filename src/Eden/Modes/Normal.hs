module Eden.Modes.Normal
    ( normalMode
    , openLine
    , delChar
    , loadFile
    ) where

import Eden.Movements
import Eden.Types
import Eden.Utils

import Data.List (intercalate)
import Control.Monad
import Data.Map (Map)

import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import qualified Yi.Rope as Y


normalMode :: Eden World ()
normalMode = do
    result <- liftIO getChar
    if result == ':'
       then do
           line <- liftIO getLine
           liftM2 (commands M.!) head tail $ words line
       else nnoremap M.! result

nnoremap :: Map Char (Eden World ())
nnoremap = M.fromList
    [ ('j', withCurBuffer down)
    , ('k', withCurBuffer up)
    , ('h', withCurBuffer $ proclaims cursorX (subtract 1))
    , ('l', withCurBuffer $ proclaims cursorX (+ 1))
    , ('w', withCurBuffer word)
    , ('O', openLine $ return ())
    , ('o', openLine down)
    , ('0', withCurBuffer jumpStart)
    , ('$', withCurBuffer jumpEnd)
    , ('x', withCurBuffer delChar)
    , ('i', proclaim wMode INSERT)
    , ('\x1b', proclaim wMode NORMAL)
    ]

openLine :: Movement -> Eden World ()
openLine dir = do
    withCurBuffer $ do
        dir
        proclaims bLines (Z.insert $ Y.fromString "")
    proclaim wMode INSERT

delChar :: Eden Buffer ()
delChar = do
    (x,_) <- inspect bCursor
    proclaims bCurLine $ delete x 1

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

