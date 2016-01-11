{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleInstances, MultiParamTypeClasses,
             DeriveFunctor, ImpredicativeTypes, LambdaCase,
             DeriveDataTypeable #-}

module Control.Monad.Jail
    ( Jail ()
    , JailT ()
    , runJail
    , runJailT
    , jail
    , jailMaybe
    , freedom
    , overwrite
    , parole
    , inquire
    , inspect
    , arrest
    , arrests
    , RLens
    ) where

import Control.Applicative (Alternative(..), Applicative(..), (<$>))
import Control.Monad (ap, liftM2)
import Control.Monad.State
import Control.Monad.Trans
import Control.Lens
import Data.Maybe (fromJust)
import Data.Typeable
import Debug.Trace


type RLens r s = Lens s s r r

newtype JailT s r m a =
    JailT
    { runJailT' :: Bool
                -> RLens r s
                -> s
                -> m (a, s, RLens r s, Bool) }
    deriving (Functor, Typeable)

instance (Applicative m, Monad m) => Applicative (JailT s r m) where
    pure x = JailT $ \v l s -> pure (x, s, l, v)
    (<*>)  = ap

instance (Applicative m, Monad m) => Monad (JailT s r m) where
    return   = pure
    ac >>= k = JailT $ \v l s ->
        if v
            then do
                (x, st', l', v') <- runJailT' ac v l s
                runJailT' (k x) v' l' st'
            else return (undefined, s, l, v)

instance (Applicative m, Monad m) => MonadState r (JailT s r m) where
    get   = JailT $ \v l s -> return (view l s, s, l, v)
    put x = JailT $ \v l s -> return ((), set l x s, l, v)

instance MonadTrans (JailT s r) where
    lift x = JailT $ \v l s -> x >>= (\a -> return (a, s, l, v))

instance (Applicative m, MonadIO m) => MonadIO (JailT s r m) where
    liftIO = lift . liftIO

instance (Applicative m, Monad m) => MonadPlus (JailT s r m) where
    mplus x y = JailT $ \v l s -> do
        x'@(xa, xs, xl, v') <- runJailT' x v l s
        if v'
           then return x'
           else runJailT' y v l s
    mzero = JailT $ \v l s -> return (undefined, s, l, False)

instance (Applicative m, Monad m) => Alternative (JailT s r m) where
    (<|>) = mplus
    empty = mzero

runJailT :: (Functor m)
         => JailT s s m a
         -> s
         -> m (Maybe (a, s))
runJailT ac = fmap (\(a, s, _, v) ->
                            if v then Just (a, s) else Nothing)
                    . runJailT' ac True id

type Jail s r = JailT s r Identity
runJail :: Jail s s a
        -> s
        -> Maybe (a, s)
runJail ac = runIdentity . runJailT ac

jail :: (Monad m)
     => RLens r' r
     -> JailT s r' m a
     -> JailT s r m a
jail l' m = JailT $ \v l s -> do
    (a, s', _, v') <- runJailT' m v (l . l') s
    return (a, s', l, v')

into :: RLens a (Maybe a)
into = lens fromJust (const Just)

jailMaybe :: (Applicative m, Monad m)
          => RLens (Maybe r') r
          -> a
          -> JailT s r' m a
          -> JailT s r m a
jailMaybe l d m =
    (gets $ view l) >>= \case
        Just _  -> jail (l . into) m
        Nothing -> return d

freedom :: (Applicative m, Monad m) => JailT s r m s
freedom = JailT $ \v l s -> return (s, s, l, v)

overwrite :: (Applicative m, Monad m)
          => RLens r' r
          -> JailT r r' m a
          -> JailT s r m a
overwrite l' m = JailT $ \v l s -> do
    (a, s', _, v') <- runJailT' m v l' $ view l s
    return (a, set l s' s, l, v')

parole :: (Applicative m, Monad m)
       => a
       -> JailT s r m (Maybe a)
       -> JailT s r m a
parole d m = do
    this <- get
    m >>= \case
        Just a  -> return a
        Nothing -> put this >> return d

inspect :: (Applicative m, Monad m)
        => RLens i r
        -> JailT s r m i
inspect l = gets $ view l

inquire :: (Applicative m, Monad m)
        => RLens i s
        -> JailT s r m i
inquire l = view l <$> freedom

arrest :: (Applicative m, Monad m)
       => RLens r' r
       -> r'
       -> JailT s r m ()
arrest l v = do
    state <- get
    put $ set l v state

arrests :: (Applicative m, Monad m)
        => RLens r' r
        -> (r' -> r')
        -> JailT s r m ()
arrests l f = do
    state <- get
    put $ set l (f $ view l state) state
