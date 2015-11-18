module Eden.Modes
    ( modes
    , insertMode
    , module Eden.Modes.Normal
    ) where

import Eden.Modes.Normal
import Eden.Types
import Eden.Utils

import Control.Monad
import Data.Map (Map)

import qualified Data.Map as M
import qualified Yi.Rope as Y

modes :: Map Mode (Eden World ())
modes = M.fromList
    [ (NORMAL, normalMode)
    , (INSERT, insertMode)
    ]

-- TODO(maguirea): enter breaks our cursor sync
insertMode :: Eden World ()
insertMode = do
    char <- liftIO getChar
    if char == '\x1b'
        then proclaim wMode NORMAL
        else withCurBuffer $ do
                 (x,_) <- inspect bCursor
                 proclaims bCurLine $ insert x (Y.fromString [char])
                 proclaims cursorX (+ 1)
