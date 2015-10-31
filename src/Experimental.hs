{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             DeriveFunctor, RankNTypes, ImpredicativeTypes #-}

module Ratchet
    ( Ratchet ()
    , RatchetT ()
    , runRatchetT
    , tighten
    ) where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM2)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Lens

type RLens r s = Lens s s r r

newtype RatchetT s r m a =
    RatchetT
    { runRatchetT' :: RLens r s -> s -> m (a, s, RLens r s) }
    deriving (Functor)

instance (Applicative m, Monad m) => Applicative (RatchetT s r m) where
    pure x = RatchetT $ \l s -> pure (x, s, l)
    (<*>) = ap

instance (Applicative m, Monad m) => Monad (RatchetT s r m) where
    return = pure
    ac >>= k = RatchetT $ \l s -> do
        (x, st', l') <- runRatchetT' ac l s
        runRatchetT' (k x) l' st'

instance (Applicative m, Monad m) => MonadReader s (RatchetT s r m) where
    ask = RatchetT $ \l s -> return (s, s, l)
    local = error "fuck"

instance (Applicative m, Monad m) => MonadState r (RatchetT s r m) where
    get = RatchetT $ \l s -> return (view l s, s, l)
    put v = RatchetT $ \l s -> return ((), set l v s, l)
    state = error "fuck"

instance MonadTrans (RatchetT s r) where
    lift x = RatchetT $ \l s -> x >>= (\a -> return (a, s, l))

instance (Applicative m, MonadIO m) => MonadIO (RatchetT s r m) where
    liftIO = lift . liftIO

runRatchetT :: (Functor m) => RatchetT s s m a -> s -> m (a, s)
runRatchetT ac = fmap (\(a, s, l) -> (a, s)) . runRatchetT' ac id

type Ratchet s r = RatchetT s r Identity

runRatchet :: Ratchet s s a -> s -> (a, s)
runRatchet ac = runIdentity . runRatchetT ac

tighten :: (Monad m) => RLens r' r -> RatchetT s r' m a -> RatchetT s r m a
tighten l' m = RatchetT $ \l s -> do
    (a, s', _) <- runRatchetT' m (l . l') s
    return (a, s', l)

