{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             DeriveFunctor, RankNTypes, ImpredicativeTypes #-}

module Jurisdiction
    ( Jurisdiction ()
    , JurisdictionT ()
    , runJurisdiction
    , runJurisdictionT
    , restrict
    ) where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM2)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Lens

type RLens r s = Lens s s r r

newtype JurisdictionT s r m a =
    JurisdictionT
    { runJurisdictionT' :: RLens r s -> s -> m (a, s, RLens r s) }
    deriving (Functor)

instance (Applicative m, Monad m) => Applicative (JurisdictionT s r m) where
    pure x = JurisdictionT $ \l s -> pure (x, s, l)
    (<*>)  = ap

instance (Applicative m, Monad m) => Monad (JurisdictionT s r m) where
    return   = pure
    ac >>= k = JurisdictionT $ \l s -> do
        (x, st', l') <- runJurisdictionT' ac l s
        runJurisdictionT' (k x) l' st'

instance (Applicative m, Monad m) => MonadReader s (JurisdictionT s r m) where
    ask   = JurisdictionT $ \l s -> return (s, s, l)
    local = error "Can't call local on a Jurisdiction"

instance (Applicative m, Monad m) => MonadState r (JurisdictionT s r m) where
    get   = JurisdictionT $ \l s -> return (view l s, s, l)
    put v = JurisdictionT $ \l s -> return ((), set l v s, l)

instance MonadTrans (JurisdictionT s r) where
    lift x = JurisdictionT $ \l s -> x >>= (\a -> return (a, s, l))

instance (Applicative m, MonadIO m) => MonadIO (JurisdictionT s r m) where
    liftIO = lift . liftIO

runJurisdictionT :: (Functor m) => JurisdictionT s s m a -> s -> m (a, s)
runJurisdictionT ac = fmap (\(a, s, l) -> (a, s)) . runJurisdictionT' ac id

type Jurisdiction s r = JurisdictionT s r Identity

runJurisdiction :: Jurisdiction s s a -> s -> (a, s)
runJurisdiction ac = runIdentity . runJurisdictionT ac

restrict :: (Monad m) => RLens r' r -> JurisdictionT s r' m a -> JurisdictionT s r m a
restrict l' m = JurisdictionT $ \l s -> do
    (a, s', _) <- runJurisdictionT' m (l . l') s
    return (a, s', l)

