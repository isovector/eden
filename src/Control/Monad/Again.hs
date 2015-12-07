{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.Again
    ( Again ()
    , again
    , runAgain
    ) where

import Control.Applicative (Applicative(..))
import Data.Dynamic
import Data.Maybe (fromJust)
import Control.Monad.RWS

-- | A monad transformer adding the ability to record the results
-- of IO actions and later replay them.
newtype Again m a =
    Again { runAgain' :: RWST () [Dynamic] [Dynamic] m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState  [Dynamic]
             , MonadWriter [Dynamic]
             , MonadTrans
             )

-- | Removes the first element from a list State and returns it.
dequeue :: MonadState [r] m
        => m (Maybe r)
dequeue = do
    get >>= \case
        []     -> return Nothing
        (x:xs) -> do
            put xs
            return $ Just x

-- | Marks an IO action to be memoized after its first invocation.
again :: ( MonadIO m
         , Typeable r)
      => IO r
      -> Again m r
again action = do
    a <- dequeue >>= \case
        Just x  -> return . fromJust $ fromDynamic x
        Nothing -> liftIO action
    tell [toDyn a]
    return a

-- | Runs an action and records all of its sampled IO. Returns a
-- action which when invoked will use the recorded IO.
runAgain :: Monad m
         => Again m a
         -> m (m a)
runAgain action = do
    (a, w) <- evalRWST (runAgain' action) () []
    return $ do
        evalRWST (runAgain' action) () w
        return a
