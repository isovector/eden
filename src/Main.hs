{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.RWS
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Yi.Rope as Y
import System.IO

type Pos = (Int, Int)

data Buffer =
    Buffer
    { bFilename :: FilePath
    , bCursor   :: Pos
    , bContent  :: Y.YiString
    }

emptyBuffer = Buffer "<unknown>" (0,0) (Y.fromString "")

newtype Eden a =
    Eden { runEden' :: RWST () () Buffer IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader ()
             , MonadWriter ()
             , MonadState Buffer
             , MonadIO
             )

runEden :: Buffer -> Eden a -> IO Buffer
runEden st e = fst <$> execRWST (runEden' e) () st

mode :: IO String
mode = return "normal"

loadFile :: FilePath -> Eden ()
loadFile f = do
    result <- liftIO $ Y.readFile f
    put . Buffer f (0, 0) $ case result of
        Right (text, _) -> text
        Left _          -> error "bad file"

commands :: Map String ([String] -> Eden ())
commands = M.fromList
    [ (":e", \xs -> loadFile $ head xs)
    ]

prompt :: Eden ()
prompt = do
    liftIO $ hSetBuffering stdout NoBuffering
    result <- liftIO $ do
        putStr =<< mode
        putStr "> "
        getLine
    liftM2 (commands M.!) head tail $ words result


main :: IO ()
main = putStrLn . Y.toString . bContent =<< runEden emptyBuffer prompt

