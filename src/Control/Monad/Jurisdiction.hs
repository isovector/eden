{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             DeriveFunctor, RankNTypes, ImpredicativeTypes, LambdaCase #-}

module Control.Monad.Jurisdiction
    ( Jurisdiction ()
    , JurisdictionT ()
    , runJurisdiction
    , runJurisdictionT
    , restrict
    , restrictInto
    , inquire
    , inspect
    , proclaim
    , proclaims
    , proclaimm
    , RLens
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (ap, liftM2)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Lens
import Data.Maybe (fromJust)


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

runJurisdictionT :: (Functor m)
                 => JurisdictionT s s m a
                 -> s
                 -> m (a, s)
runJurisdictionT ac = fmap (\(a, s, l) -> (a, s)) . runJurisdictionT' ac id

type Jurisdiction s r = JurisdictionT s r Identity
runJurisdiction :: Jurisdiction s s a
                -> s
                -> (a, s)
runJurisdiction ac = runIdentity . runJurisdictionT ac

restrict :: (Monad m)
         => RLens r' r
         -> JurisdictionT s r' m a
         -> JurisdictionT s r m a
restrict l' m = JurisdictionT $ \l s -> do
    (a, s', _) <- runJurisdictionT' m (l . l') s
    return (a, s', l)

into :: RLens a (Maybe a)
into = lens fromJust (const Just)

restrictInto :: (Applicative m, Monad m)
             => RLens (Maybe r') r
             -> JurisdictionT s r' m ()
             -> JurisdictionT s r m ()
restrictInto l m =
    (gets $ view l) >>= \case
        Just _  -> restrict (l . into) m
        Nothing -> return ()

inspect :: (Applicative m, Monad m)
        => RLens i r
        -> JurisdictionT s r m i
inspect l = gets $ view l

inquire :: (Applicative m, Monad m)
        => RLens i s
        -> JurisdictionT s r m i
inquire l = ask $ view l

proclaim :: (Applicative m, Monad m)
         => RLens r' r
         -> r'
         -> JurisdictionT s r m ()
proclaim l v = do
    state <- get
    put $ set l v state

proclaims :: (Applicative m, Monad m)
         => RLens r' r
         -> (r' -> r')
         -> JurisdictionT s r m ()
proclaims l f = do
    state <- get
    put $ set l (f $ view l state) state

proclaimm :: (Applicative m, Monad m)
          => RLens r' r
          -> (r' -> r')
          -> JurisdictionT s (Maybe r) m ()
proclaimm l f = do
    get >>= \case
        Just x  -> put . Just $ set l (f $ view l x) x
        Nothing -> return ()
