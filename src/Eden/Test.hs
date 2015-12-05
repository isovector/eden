{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Monad.RWS
import Data.IORef
import System.IO.Unsafe

type MemoT k r m a = StateT (Map k r) m a

keep :: ( Functor m
        , MonadIO m
        , Ord k
        , MonadState (Map k a) m)
     => k
     -> IO a
     -> m a
keep k x =
    M.lookup k <$> get >>= \case
      Just y  -> return y
      Nothing -> do
          r <- liftIO x
          modify $ M.insert k r
          return r

lose :: ( Ord k
        , MonadState (Map k a) m
        )
     => k
     -> m ()
lose k = modify $ M.delete k

disregard :: MonadState (Map k a) m => m ()
disregard = put M.empty

runMemoT :: Functor m => MemoT k r m a -> m a
runMemoT = fmap fst . flip runStateT M.empty


type ReplayT m a = RWST () [Dynamic] [Dynamic] m a
dequeue :: MonadState [r] m => m (Maybe r)
dequeue = do
    get >>= \case
        []     -> return Nothing
        (x:xs) -> do
            put xs
            return $ Just x

sample :: Typeable r => MonadIO m => IO r -> ReplayT m r
sample action = do
    a <- dequeue >>= \case
        Just x  -> return . fromJust $ fromDynamic x
        Nothing -> liftIO action
    tell [toDyn a]
    return a

record :: Monad m => ReplayT m a -> m (m a)
record action = do
    (a, w) <- evalRWST action () []
    return $ do
        evalRWST action () w
        return a

testReplay :: ReplayT IO ()
testReplay = do
    first <- sample $ do
        putStr "first> "
        getLine
    last <- sample $ do
        putStr "last> "
        getLine
    liftIO $ do
        putStr "hello "
        putStr first
        putStr " "
        putStrLn last
    return ()

runReplay :: IO String
runReplay = do
    r <- record testReplay
    r
    r
    return ""


test :: MemoT String String IO String
test = do
    first <- keep "first" $ do
        putStr "first> "
        getLine
    last <- keep "last" $ do
        putStr "last> "
        getLine
    liftIO $ do
        putStr "hello "
        putStr first
        putStr " "
        putStrLn last
    return ""

runTest :: IO String
runTest = runMemoT $ do
    test
    test
    lose "first"
    test
    return "done"

-- Memoize an IO function
memoIO :: MonadIO m => m a -> m (m a)
memoIO action = do
  ref <- liftIO $ newMVar Nothing
  return $ do
      x <- maybe action return =<< liftIO (takeMVar ref)
      liftIO . putMVar ref $ Just x
      return x

-- Global variable to contain the action we want to repeat
actionToRepeat :: IORef (IO String)
actionToRepeat = unsafePerformIO . newIORef $ return ""

-- Run an action and store it as the action to repeat
repeatable :: IO String -> IO String
repeatable action = do
    writeIORef actionToRepeat action
    action

-- Run the last action stored by repeatable
doRepeat :: IO String
doRepeat = do
    x <- readIORef actionToRepeat
    x

-- IO function to memoize
getName :: IO String
getName = do
    putStr "name> "
    getLine

-- Expected output:
-- name> isovector
-- hello isovector
-- hello isovector
main :: IO ()
main = do
    memoized <- memoIO getName
    repeatable $ do
        name <- memoized
        putStr "hello "
        putStrLn name
        return name
    doRepeat
    return ()

