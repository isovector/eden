module Eden.TextObjs where

import Eden.Motions
import Eden.Operators
import Eden.Types

import Control.Arrow (second)
import Data.Map (Map)

import qualified Data.Map as M

textobjs :: Map String (Eden World TextObj)
textobjs = M.fromList $
    [ 
    -- ("aw", aWord)
    ] ++ map (second liftMotion) (M.toList motions)

