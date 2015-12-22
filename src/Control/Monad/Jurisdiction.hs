{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             DeriveFunctor, RankNTypes, ImpredicativeTypes, LambdaCase,
             DeriveDataTypeable #-}

module Control.Monad.Jurisdiction
    ( Jurisdiction ()
    , JurisdictionT ()
    , runJurisdiction
    , runJurisdictionT
    , restrict
    , restrictInto
    , overwrite
    , vote
    , inquire
    , inspect
    , proclaim
    , proclaims
    , RLens
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (ap, liftM2)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Lens
import Data.Maybe (fromJust)
import Data.Typeable
import Debug.Trace


type RLens r s = Lens s s r r

newtype JurisdictionT s r m a =
    JurisdictionT
    { runJurisdictionT' :: Bool
                        -> RLens r s
                        -> s
                        -> m (a, s, RLens r s, Bool) }
    deriving (Functor, Typeable)

instance (Applicative m, Monad m) => Applicative (JurisdictionT s r m) where
    pure x = JurisdictionT $ \v l s -> pure (x, s, l, v)
    (<*>)  = ap

instance (Applicative m, Monad m) => Monad (JurisdictionT s r m) where
    return   = pure
    ac >>= k = JurisdictionT $ \v l s ->
        if v
            then do
                (x, st', l', v') <- runJurisdictionT' ac v l s
                runJurisdictionT' (k x) v' l' st'
            else return (undefined, s, l, v)

instance (Applicative m, Monad m) => MonadReader s (JurisdictionT s r m) where
    ask   = JurisdictionT $ \v l s -> return (s, s, l, v)
    local = error "Can't call local on a Jurisdiction"

instance (Applicative m, Monad m) => MonadState r (JurisdictionT s r m) where
    get   = JurisdictionT $ \v l s -> return (view l s, s, l, v)
    put x = JurisdictionT $ \v l s -> return ((), set l x s, l, v)

instance MonadTrans (JurisdictionT s r) where
    lift x = JurisdictionT $ \v l s -> x >>= (\a -> return (a, s, l, v))

instance (Applicative m, MonadIO m) => MonadIO (JurisdictionT s r m) where
    liftIO = lift . liftIO

instance (Applicative m, Monad m) => MonadPlus (JurisdictionT s r m) where
    mplus x y = JurisdictionT $ \v l s -> do
        x'@(xa, xs, xl, v') <- runJurisdictionT' x v l s
        if v'
           then return x'
           else runJurisdictionT' y v l s
    mzero = JurisdictionT $ \v l s -> return (undefined, s, l, False)

runJurisdictionT :: (Functor m)
                 => JurisdictionT s s m a
                 -> s
                 -> m (Maybe (a, s))
runJurisdictionT ac = fmap (\(a, s, _, v) ->
                            if v then Just (a, s) else Nothing)
                    . runJurisdictionT' ac True id

type Jurisdiction s r = JurisdictionT s r Identity
runJurisdiction :: Jurisdiction s s a
                -> s
                -> Maybe (a, s)
runJurisdiction ac = runIdentity . runJurisdictionT ac

restrict :: (Monad m)
         => RLens r' r
         -> JurisdictionT s r' m a
         -> JurisdictionT s r m a
restrict l' m = JurisdictionT $ \v l s -> do
    (a, s', _, v') <- runJurisdictionT' m v (l . l') s
    return (a, s', l, v')

into :: RLens a (Maybe a)
into = lens fromJust (const Just)

restrictInto :: (Applicative m, Monad m)
             => RLens (Maybe r') r
             -> a
             -> JurisdictionT s r' m a
             -> JurisdictionT s r m a
restrictInto l d m =
    (gets $ view l) >>= \case
        Just _  -> restrict (l . into) m
        Nothing -> return d

overwrite :: (Applicative m, Monad m)
          => RLens r' r
          -> JurisdictionT r r' m a
          -> JurisdictionT s r m a
overwrite l' m = JurisdictionT $ \v l s -> do
    (a, s', _, v') <- runJurisdictionT' m v l' $ view l s
    return (a, set l s' s, l, v')

vote :: (Applicative m, Monad m)
     => a
     -> JurisdictionT s r m (Maybe a)
     -> JurisdictionT s r m a
vote d m = do
    this <- get
    m >>= \case
        Just a  -> return a
        Nothing -> put this >> return d

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
