{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             DeriveFunctor, RankNTypes, ImpredicativeTypes #-}

module Ratchet
    ( ratchet
    , runRatchet
    , tighten
    ) where

import Control.Applicative (Applicative(..))
import Control.Monad (ap)
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Control.Lens.TH

type RLens r s = Lens s s r r

newtype Ratchet s r a =
    Ratchet
    { runRatchet' :: RLens r s -> s -> (a, s, RLens r s) }
    deriving (Functor)

ratchet :: (RLens r s -> s -> (a, s, RLens r s)) -> Ratchet s r a
ratchet = Ratchet

runRatchet :: Ratchet s s a -> s -> (a, s)
runRatchet m = (\(a, s, l) -> (a, s)) . runRatchet' m id

instance Applicative (Ratchet s r) where
    pure x = ratchet $ \l s -> (x, s, l)
    (<*>) = ap

instance Monad (Ratchet s r) where
    return = pure
    ac >>= k = ratchet $ \l s ->
        let (x, st', l') = runRatchet' ac l s
         in runRatchet' (k x) l' st'

instance MonadReader s (Ratchet s r) where
    ask = ratchet $ \l s -> (s, s, l)
    local = error "fuck"

instance MonadState r (Ratchet s r) where
    get = ratchet $ \l s -> (view l s, s, l)
    put v = ratchet $ \l s -> ((), set l v s, l)
    state = error "fuck"

tighten :: RLens r' r -> Ratchet s r' a -> Ratchet s r a
tighten l' m = ratchet $ \l s ->
    let (a, s', _) = runRatchet' m (l . l') s
     in (a, s', l)

