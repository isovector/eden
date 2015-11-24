{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             LambdaCase #-}

module Eden.Types ( CurPos
                  , Mode (..)
                  , Eden
                  , Motion
                  , Wiseness (..)
                  , TextObj (..)

                  , Mark (..)
                  , markX
                  , markY

                  , Buffer (..)
                  , emptyBuffer
                  , bFilename
                  , bCursor
                  , bLines
                  , bCurLine

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

import Control.Lens
import Control.Lens.TH
import Control.Monad.Jurisdiction
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.IntMap (IntMap)
import Data.List.Zipper (Zipper)
import Data.Map (Map)

import qualified Data.IntMap      as I
import qualified Data.List.Zipper as Z
import qualified Data.Map         as M
import qualified Yi.Rope          as Y


type CurPos = (Int, Int)

data Mode = NORMAL | INSERT deriving (Eq, Show, Read, Ord)

data Buffer =
    Buffer
    { _bFilename :: FilePath
    , _bCursor   :: CurPos
    , _bLines    :: Zipper Y.YiString
    }
makeLenses ''Buffer
emptyBuffer = Buffer "[No Name]" (0,0) Z.empty

curLine :: RLens a (Zipper a)
curLine = lens Z.cursor (flip Z.insert . Z.delete)

bCurLine :: RLens Y.YiString Buffer
bCurLine = bLines . curLine


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

type Motion = Eden Buffer ()

data Mark = Mark Int Int deriving (Eq, Ord, Show)

markX :: RLens Int Mark
markX = lens (\(Mark x _) -> x) (\(Mark _ y) x -> Mark x y)

markY :: RLens Int Mark
markY = lens (\(Mark _ y) -> y) (\(Mark x _) y -> Mark x y)

data Wiseness = Charwise
              | Linewise
              | Blockwise
              deriving (Eq, Show)

data TextObj = TextObj Wiseness Mark Mark
    deriving (Eq, Show)

