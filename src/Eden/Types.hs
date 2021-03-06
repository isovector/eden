{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell,
             FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
             LambdaCase, DeriveDataTypeable #-}

module Eden.Types ( CurPos
                  , Mode (..)
                  , Eden
                  , Repeatable
                  , Direction (..)
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
                  , wRepeated
                  , wRepMotion

                  , module Control.Monad.Jail
                  , module Control.Monad.Again
                  , (<$>)
                  , view, set
                  , lift, liftIO
                  , ask, asks
                  , get, gets, put
                  ) where

import Control.Applicative ((<$>))
import Control.Lens
import Control.Lens.TH
import Control.Monad.Jail
import Control.Monad.Reader
import Control.Monad.Again
import Control.Monad.State
import Control.Monad.Trans
import Data.IntMap (IntMap)
import Data.List.Zipper (Zipper)
import Data.Map (Map)
import Data.Typeable

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
    } deriving (Typeable)
makeLenses ''Buffer
emptyBuffer = Buffer "[No Name]" (0,0) Z.empty

curLine :: RLens a (Zipper a)
curLine = lens Z.cursor (flip Z.insert . Z.delete)

bCurLine :: RLens Y.YiString Buffer
bCurLine = bLines . curLine

data Direction = Backwards | Forwards

data World =
    World
    { _wBuffers :: IntMap Buffer
    , _wMode    :: Mode
    , _wCurBuffer :: Int
    , _wNextBuffer :: Int
    , _wRepeated :: JailT World Buffer IO ()
    , _wRepMotion :: ReaderT Direction (JailT World Buffer IO) ()
    } deriving (Typeable)
makeLenses ''World
emptyWorld = World I.empty NORMAL 0 0 (return ()) $ return ()

type Eden r = JailT World r IO
type Repeatable s = Again (Eden s)

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
    deriving (Eq, Show, Typeable)

