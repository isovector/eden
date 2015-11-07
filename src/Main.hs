{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             LambdaCase #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Jurisdiction
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Lens
import Control.Lens.TH
import Control.Lens.Zoom
import Control.Lens.Internal.Zoom (Zoomed, FocusingWith)
import Data.Either (rights)
import Data.List (intercalate)
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.Map (Map)
import qualified Data.Map as M
import qualified Yi.Rope as Y
import System.Console.ANSI
import System.IO

type Pos = (Int, Int)

data Mode = NORMAL | INSERT deriving (Eq, Show, Read, Ord)

data Buffer =
    Buffer
    { _bFilename :: FilePath
    , _bCursor   :: Pos
    , _bContent  :: Y.YiString
    }
makeLenses ''Buffer
emptyBuffer = Buffer "[No Name]" (0,0) (Y.fromString "")

data World =
    World
    { _wBuffers :: IntMap Buffer
    , _wMode    :: Mode
    , _wCurBuffer :: Int
    , _wNextBuffer :: Int
    }
makeLenses ''World
emptyWorld = World I.empty NORMAL 0 0

-- newtype Eden r a =
--     Eden { runEden' :: JurisdictionT World r IO a }
--     deriving ( Functor
--              , Applicative
--              , Monad
--              , MonadReader World
--              , MonadState r
--              , MonadIO
--              )

type Eden r a = JurisdictionT World r IO a
runEden = runJurisdictionT

loadFile :: FilePath -> Eden (Maybe Buffer) ()
loadFile f = do
    result <- liftIO $ Y.readFile f
    put . Just . Buffer f (0, 0) $ case result of
        Right (text, _) -> text
        Left _          -> error "bad file"

withNextBuffer :: Eden (Maybe Buffer) a -> Eden World a
withNextBuffer n = do
    next   <- inquire wNextBuffer
    result <- restrict (wBuffers . at next) n
    proclaims wNextBuffer (+1)
    return result

withCurBuffer :: Eden Buffer () -> Eden World ()
withCurBuffer n = do
    current <- inquire wCurBuffer
    restrictInto (wBuffers . at current) n

curBuffer :: Eden a (Maybe Buffer)
curBuffer = do
    current <- inquire wCurBuffer
    inquire (wBuffers . at current)

modes :: Map Mode (Eden World ())
modes = M.fromList
    [ (NORMAL, normalMode)
    , (INSERT, insertMode)
    ]

normalMode :: Eden World ()
normalMode = do
    result <- liftIO getChar
    if result == ':'
       then do
           line <- liftIO getLine
           liftM2 (commands M.!) head tail $ words line
       else nnoremap M.! result

withLines :: ((Int, Y.YiString) -> Y.YiString) -> Y.YiString -> Y.YiString
withLines f = Y.unlines . map f . zip [0..] . Y.lines

insert :: Pos -> Y.YiString -> Y.YiString -> Y.YiString
insert (cx, cy) what s = withLines go s
    where go (y, line) = if y == cy
                             then yInsert cx what line
                             else line

yInsert :: Int -> Y.YiString -> Y.YiString -> Y.YiString
yInsert x what line = let (left,right) = Y.splitAt x line
                       in Y.concat [left, what, right]

insertMode :: Eden World ()
insertMode = do
    char <- liftIO getChar
    if char == '\x1b'
        then proclaim wMode NORMAL
        else withCurBuffer $ do
                 cursor <- gets $ view bCursor
                 proclaims bContent $ insert cursor (Y.fromString [char])
                 proclaims (bCursor . _1) (+ 1)

nnoremap :: Map Char (Eden World ())
nnoremap = M.fromList
    [ ('j', withCurBuffer $ proclaims (bCursor . _2) (+ 1))
    , ('k', withCurBuffer $ proclaims (bCursor . _2) (subtract 1))
    , ('h', withCurBuffer $ proclaims (bCursor . _1) (subtract 1))
    , ('l', withCurBuffer $ proclaims (bCursor . _1) (+ 1))
    , ('i', proclaim wMode INSERT)
    , ('\x1b', proclaim wMode NORMAL)
    ]

display :: Buffer -> IO ()
display b = do
    let clines = Y.lines $ view bContent b
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

asWords :: (String -> b) -> ([String] -> b)
asWords f = f . intercalate " "

commands :: Map String ([String] -> Eden World ())
commands = M.fromList
    [ ("e", asWords $ withNextBuffer . loadFile)
    ]

input :: Eden World ()
input = do
    result <- liftIO getChar
    if result == ':'
       then do
           line <- liftIO getLine
           liftM2 (commands M.!) head tail $ words line
       else nnoremap M.! result


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
        putStr . show . I.size $ _wBuffers world
        putStr "> "
        hFlush stdout
    modes M.! mode

main :: IO ()
main = do
    s <- snd <$> runEden (forever prompt) emptyWorld
    seq s $ return ()



