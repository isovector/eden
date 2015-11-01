{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

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
    { _wBuffers :: [Buffer]
    , _wMode    :: String
    }
makeLenses ''World
emptyWorld = World [] "normal"

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

loadFile :: FilePath -> Eden [Buffer] ()
loadFile f = do
    result <- liftIO $ Y.readFile f
    buffers <- get
    put . (: buffers) . Buffer f (0, 0) $ case result of
        Right (text, _) -> text
        Left _          -> error "bad file"

withBuffers :: Eden [Buffer] a -> Eden World a
withBuffers = restrict wBuffers

commands :: Map String ([String] -> Eden World ())
commands = M.fromList
    [ (":e", withBuffers . loadFile . intercalate " ")
    , (":",  const $ return ())
    , (":mode", restrict wMode . put . intercalate " ")
    ]

prompt :: Eden World ()
prompt = do
    world  <- get
    mode   <- inquire wMode
    result <- liftIO $ do
        putStr mode
        putStr " "
        putStr . show . length $ _wBuffers world
        putStr "> "
        getLine
    liftM2 (commands M.!) head tail $ words result

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    s <- snd <$> runEden (forever prompt) emptyWorld
    seq s $ return ()

