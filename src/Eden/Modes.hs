module Eden.Modes
    ( modes
    , insertMode
    , normalMode
    , loadFile
    , module Eden.Modes.Normal
    ) where

import Eden.Modes.Normal
import Eden.TextObjs
import Eden.Motions
import Eden.Operators
import Eden.Types
import Eden.Utils

import Control.Arrow (second)
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

insertMode :: Eden World ()
insertMode = do
    char <- liftIO getChar
    case char of
      '\x1b' -> proclaim wMode NORMAL
      '\n'   ->
          withCurBuffer $ do
              x <- inspect cursorX
              proclaims bLines (lineBreak x)
              proclaims cursorY (+ 1)
              proclaim cursorX 0
      _      ->
          withCurBuffer $ do
              x <- inspect cursorX
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
    , ('J', withCurBuffer joinLine)
    , ('d', operator deleteOp)
    , ('c', operator changeOp)
    , ('D', operateToEnd deleteOp)
    , ('C', operateToEnd changeOp)
    , ('i', proclaim wMode INSERT)
    , ('\x1b', proclaim wMode NORMAL)
    ] ++ map toNMap (M.toList motions)
  where
    toNMap (key, motion) = (head key, withCurBuffer $ (>> sanitizeCursor) motion)

commands :: Map String ([String] -> Eden World ())
commands = M.fromList
    [ ("e", asWords $ withNextBuffer . loadFile)
    ]

loadFile :: FilePath -> Eden (Maybe Buffer) ()
loadFile f = do
    result <- liftIO $ Y.readFile f
    put . Just . Buffer f (0, 0) $ case result of
        Right (text, _) -> Z.fromList $ Y.lines text
        -- TODO(sandy): make this maybe not crash?
        Left _          -> error "bad file"

asWords :: (String -> b) -> ([String] -> b)
asWords f = f . intercalate " "

