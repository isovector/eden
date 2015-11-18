module Eden.Operators where

import Eden.Marks
import Eden.Modes.Normal
import Eden.Movements
import Eden.Types

data Wiseness = Charwise
              | Linewise
              | Blockwise

type Operator = Wiseness -> Mark -> Mark -> Eden Buffer ()


runOperator :: Operator -> Movement -> Eden Buffer ()
runOperator op m = do
    here <- getMark
    m
    there <- getMark
    jumpToMark here
    op Charwise here there

deleteOp :: Operator
deleteOp w b e = do
    jumpToMark e
    prevChar
    case w of
      Charwise  -> do charwiseTowards delChar b
                      delChar
      otherwise -> error "only charwise is supported"
