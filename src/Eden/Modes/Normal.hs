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
import Data.Map (Map)

import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import qualified Yi.Rope as Y


openLine :: Motion -> Eden World ()
openLine dir = do
    withCurBuffer $ do
        dir
        proclaims bLines (Z.insert $ Y.fromString "")
    proclaim wMode INSERT

delChar :: Eden Buffer ()
delChar = do
    (x,_) <- inspect bCursor
    proclaims bCurLine $ delete x 1

joinLine :: Eden Buffer ()
joinLine = do
    jumpEnd
    proclaims bLines . lineJoin . Just $ Y.fromString " "

repeatAction :: Eden World ()
repeatAction = do
    x <- inquire wRepeated
    x

