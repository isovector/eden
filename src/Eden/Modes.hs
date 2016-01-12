{-# LANGUAGE LambdaCase #-}

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
    , (INSERT, withCurBuffer $ appendRepeat insertMode)
    ]

setMode :: Mode -> Eden World ()
setMode mode = do
    when (mode == INSERT) $
        arrest wRepeated $ return ()
    arrest wMode mode

insertMode :: Repeatable Buffer ()
insertMode = do
    char <- again $ liftIO getChar
    lift $ case char of
      '\x1b' -> escape $ setMode NORMAL
      '\n'   -> do
            x <- inspect cursorX
            arrests bLines (lineBreak x)
            arrests cursorY (+ 1)
            arrest cursorX 0
      '\127' -> do -- backspace
            -- there is a bug here for backspace at 0,0
            prevChar
            inspect cursorX >>= \case
                0 -> arrests bLines $ lineJoin Nothing
                _ -> delChar
      -- 27 is delete
      _      -> do
            x <- inspect cursorX
            arrests bCurLine $ insert x (Y.fromString [char])
            arrests cursorX (+ 1)

normalMode :: Eden World ()
normalMode = do
    result <- liftIO getChar
    if result == ':'
       then do
           line <- liftIO getLine
           liftM2 (commands M.!) head tail $ words line
       else withCurBuffer $ nnoremap M.! result

nnoremap :: Map Char (Eden Buffer ())
nnoremap = M.fromList $
    [ ('O', liftRepeat . openLine $ return ())
    , ('o', liftRepeat $ openLine down)
    , ('x', liftRepeat $ delChar)
    , ('J', liftRepeat $ joinLine)
    , ('d', repeatable $ operator deleteOp)
    , ('c', repeatable $ operator changeOp)
    , ('D', repeatable $ operateToEnd deleteOp)
    , ('C', repeatable $ operateToEnd changeOp)
    , ('t', repeatableMotion $ toChar Forwards)
    , ('f', repeatableMotion $ findChar Forwards)
    , ('T', repeatableMotion $ toChar Backwards)
    , ('F', repeatableMotion $ findChar Backwards)
    , ('s', repeatableMotion $ snipe Forwards)
    , ('S', repeatableMotion $ snipe Backwards)
    , ('.', repeatAction)
    , (';', repeatMotion Forwards)
    , (',', repeatMotion Backwards)
    , ('i', escape $ setMode INSERT)
    , ('\x1b', escape $ setMode NORMAL)
    ] ++ map toNMap (M.toList motions)
  where
    toNMap (key, motion) = ( head key
                           , try $ motion >> sanitizeCursor)

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

