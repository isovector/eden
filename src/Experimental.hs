{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             DeriveFunctor #-}
module Experimental where

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Control.Lens.TH
import Control.Lens.Zoom
import Control.Lens.Internal.Zoom (Zoomed, Focusing)

type RLens f r s = (r -> f r) -> s -> f s

newtype Restriction f s r a =
    Restriction
    { runRestriction :: RLens f r s -> s -> (a, s, RLens f r s) }
    deriving (Functor)

restriction :: (RLens f r s -> s -> (a, s, RLens f r s)) -> Restriction f s r a
restriction = Restriction

instance (Functor f) => Monad (Restriction f s r) where
    return x = restriction $ \l s -> (x, s, l)
    ac >>= k = restriction $ \l s ->
        let (x, st', l') = runRestriction ac l s
         in runRestriction (k x) l' st'

instance (Functor f) => MonadReader s (Restriction f s r) where
    ask = restriction $ \l s -> (s, s, l)
    local = error "fuck"

instance (Functor f) => MonadState r (Restriction f s r) where
    get = restriction $ \l s -> (view l s, s, l)
    put v = restriction $ \l s -> ((), set l v s, l)
    state = error "fuck"

type Pos = (Int, Int)

data Buffer =
    Buffer
    { _bFilename :: FilePath
    , _bCursor   :: Pos
    , _bContent  :: String
    }
makeLenses ''Buffer
emptyBuffer = Buffer "[No Name]" (0,0) ""

data World =
    World
    { _wMode   :: String
    , _wBuffer :: Buffer
    }
makeLenses ''World
emptyWorld = World "normal" emptyBuffer



