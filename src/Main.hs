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
import System.IO

type Pos = (Int, Int)

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
    , _wMode    :: String
    , _wCurBuffer :: Int
    , _wNextBuffer :: Int
    }
makeLenses ''World
emptyWorld = World I.empty "normal" 0 0

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

withCurBuffer :: Eden (Maybe Buffer) a -> Eden World a
withCurBuffer n = do
    current <- inquire wCurBuffer
    restrict (wBuffers . at current) n



nnoremap :: Map Char (Eden World ())
nnoremap = M.fromList
    [ ('j', withCurBuffer $ proclaimm (bCursor . _2) (+ 1))
    , ('k', withCurBuffer $ proclaimm (bCursor . _2) (subtract 1))
    , ('h', withCurBuffer $ proclaimm (bCursor . _1) (subtract 1))
    , ('l', withCurBuffer $ proclaimm (bCursor . _1) (+ 1))
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
                   in Y.concat [left, Y.cons '|' right]
             else line

asWords :: (String -> b) -> ([String] -> b)
asWords f = f . intercalate " "

commands :: Map String ([String] -> Eden World ())
commands = M.fromList
    [ ("e", asWords $ withNextBuffer . loadFile)
    , (":",  const $ return ())
    , ("mode", asWords $ restrict wMode . put)
    , ("p",
        \s -> do
            restrict (wBuffers . at (read $ concat s)) $ do
                get >>= liftIO . \case
                    Just x  -> liftIO $ display x
                    Nothing -> putStrLn "buffer does not exist"
        )
    ]

prompt :: Eden World ()
prompt = do
    world  <- get
    mode   <- inquire wMode
    result <- liftIO $ do
        hFlush stdout
        putStr mode
        putStr " "
        putStr . show . I.size $ _wBuffers world
        putStr "> "
        hFlush stdout
        getLine
    liftM2 (commands M.!) head tail $ words result

main :: IO ()
main = do
    s <- snd <$> runEden (forever prompt) emptyWorld
    seq s $ return ()

