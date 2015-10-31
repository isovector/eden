{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             DeriveFunctor, RankNTypes, ImpredicativeTypes #-}

module Ratchet
    ( Ratchet ()
    , runRatchet
    , tighten
    ) where

import Control.Applicative (Applicative(..))
import Control.Monad (ap)
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

type RLens r s = Lens s s r r

newtype Ratchet s r a =
    Ratchet
    { runRatchet' :: RLens r s -> s -> (a, s, RLens r s) }
    deriving (Functor)

instance Applicative (Ratchet s r) where
    pure x = Ratchet $ \l s -> (x, s, l)
    (<*>) = ap

instance Monad (Ratchet s r) where
    return = pure
    ac >>= k = Ratchet $ \l s ->
        let (x, st', l') = runRatchet' ac l s
         in runRatchet' (k x) l' st'

instance MonadReader s (Ratchet s r) where
    ask = Ratchet $ \l s -> (s, s, l)
    local = error "fuck"

instance MonadState r (Ratchet s r) where
    get = Ratchet $ \l s -> (view l s, s, l)
    put v = Ratchet $ \l s -> ((), set l v s, l)
    state = error "fuck"

runRatchet :: Ratchet s s a -> s -> (a, s)
runRatchet m = (\(a, s, l) -> (a, s)) . runRatchet' m id

tighten :: RLens r' r -> Ratchet s r' a -> Ratchet s r a
tighten l' m = Ratchet $ \l s ->
    let (a, s', _) = runRatchet' m (l . l') s
     in (a, s', l)

