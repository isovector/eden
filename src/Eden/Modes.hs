module Eden.Modes where

import Eden.Types
import Eden.Utils

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad
import Control.Monad.Loops (untilM_)
import Data.Char
import Data.IntMap (IntMap)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (fromJust)

import qualified Data.IntMap as I
import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import qualified Yi.Rope as Y


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

openLine :: Movement -> Eden World ()
openLine dir = do
    withCurBuffer $ do
        dir
        proclaims bLines (Z.insert $ Y.fromString "")
    proclaim wMode INSERT

type Movement = Eden Buffer ()

cursorX :: RLens Int Buffer
cursorX = bCursor . _1

cursorY :: RLens Int Buffer
cursorY = bCursor . _2

cursorChar :: Eden Buffer Char
cursorChar = do
    x <- inspect cursorX
    restrict bCurLine $ do
        (_, right) <- gets $ Y.splitAt x
        case Y.head right of
            Just x ->  return x
            Nothing -> return '\0'

up :: Movement
up = do
    z <- inspect bLines
    if not $ Z.beginp z
        then do
             proclaims cursorY (subtract 1)
             proclaims (bLines) Z.left
        else return ()

down :: Movement
down = do
    z <- inspect bLines
    if not $ Z.endp z
        then do
             proclaims cursorY (+ 1)
             proclaims (bLines) Z.right
        else return ()

jumpStart :: Movement
jumpStart = proclaim cursorX 0

jumpEnd :: Movement
jumpEnd =   subtract 1 . Y.length
        <$> inspect bCurLine >>= proclaim cursorX

nextChar :: Movement
nextChar = do
    len   <- Y.length <$> inspect bCurLine
    proclaims cursorX (+ 1)
    (x,_) <- inspect bCursor
    if x >= len
        then do
            down
            jumpStart
        else return ()

skipSpaces :: Movement
skipSpaces = do
    cur <- cursorChar
    if isSpace cur
        then nextChar `untilM_` liftM (not . isSpace) cursorChar
        else return ()

word :: Movement
word = do
    lineNum <- inspect cursorY
    skipChars <- liftM2 (||) isPunctuation isSymbol <$> cursorChar
    nextChar `untilM_` wordBoundary lineNum skipChars
    skipSpaces
  where
    wordBoundary lineNum skip = do
        cur     <- cursorChar
        curLine <- inspect cursorY
        return . (||) (curLine /= lineNum) . not
               $ if skip
                    then liftM2 (||) isPunctuation isSymbol $ cur
                    else isAlphaNum cur

delChar :: Eden Buffer ()
delChar = do
    (x,_) <- inspect bCursor
    proclaims bCurLine $ delete x 1

normalMode :: Eden World ()
normalMode = do
    result <- liftIO getChar
    if result == ':'
       then do
           line <- liftIO getLine
           liftM2 (commands M.!) head tail $ words line
       else nnoremap M.! result

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

commands :: Map String ([String] -> Eden World ())
commands = M.fromList
    [ ("e", asWords $ withNextBuffer . loadFile)
    ]
