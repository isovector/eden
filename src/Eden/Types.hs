{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             LambdaCase #-}

module Eden.Types ( Pos
                  , Mode (..)
                  , Eden

                  , Buffer (..)
                  , emptyBuffer
                  , bFilename
                  , bCursor
                  , bLines

                  , World ()
                  , emptyWorld
                  , wBuffers
                  , wMode
                  , wCurBuffer
                  , wNextBuffer

                  , module Control.Monad.Jurisdiction
                  , view, set
                  , lift, liftIO
                  , ask, asks
                  , get, gets, put
                  ) where

import Control.Monad.Jurisdiction
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Lens
import Control.Lens.TH
import Data.List.Zipper (Zipper)
import qualified Data.List.Zipper as Z
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.Map (Map)
import qualified Data.Map as M
import qualified Yi.Rope as Y


type Pos = (Int, Int)

data Mode = NORMAL | INSERT deriving (Eq, Show, Read, Ord)

data Buffer =
    Buffer
    { _bFilename :: FilePath
    , _bCursor   :: Pos
    , _bLines    :: Zipper Y.YiString
    }
makeLenses ''Buffer
emptyBuffer = Buffer "[No Name]" (0,0) Z.empty

data World =
    World
    { _wBuffers :: IntMap Buffer
    , _wMode    :: Mode
    , _wCurBuffer :: Int
    , _wNextBuffer :: Int
    }
makeLenses ''World
emptyWorld = World I.empty NORMAL 0 0

type Eden r a = JurisdictionT World r IO a

