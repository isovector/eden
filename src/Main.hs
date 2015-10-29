{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.RWS
import Data.Map (Map)
import Data.Either (rights)
import qualified Data.Map as M
import qualified Yi.Rope as Y

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
    put . Buffer f (0, 0) . fst . (!! 0) . rights $ return result

commands :: Map String ([String] -> Eden ())
commands = M.fromList
    [ (":e", \xs -> loadFile (xs !! 0))
    ]

prompt :: Eden ()
prompt = do
    result <- liftIO $ do
        putStr =<< mode
        putStr "> "
        getLine
    let parsed = words result
    commands M.! (parsed !! 0) $ tail parsed

main :: IO ()
main = putStrLn . show . bContent =<< runEden emptyBuffer prompt

