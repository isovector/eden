module Eden.Modes.Normal
    ( openLine
    , delChar
    , joinLine
    , repeatAction
    ) where

import Eden.Motions
import Eden.Types
import Eden.Utils

import Control.Monad
import Control.Monad.Reader
import Data.Map (Map)

import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import qualified Yi.Rope as Y


openLine :: Motion -> Eden Buffer ()
openLine dir = do
    dir
    arrests bLines (Z.insert $ Y.fromString "")
    escape $ arrest wMode INSERT

delChar :: Eden Buffer ()
delChar = do
    (x,_) <- inspect bCursor
    arrests bCurLine $ delete x 1

joinLine :: Eden Buffer ()
joinLine = do
    jumpEnd
    arrests bLines . lineJoin . Just $ Y.fromString " "

repeatAction :: Eden Buffer ()
repeatAction = join $ inquire wRepeated

