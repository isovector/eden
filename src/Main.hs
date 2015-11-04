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
                    Just x  -> putStrLn . Y.toString $ view bContent x
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

