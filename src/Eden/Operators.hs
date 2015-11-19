module Eden.Operators where

import Eden.Marks
import Eden.Modes.Normal
import Eden.Motions
import Eden.Types
import Eden.Utils

data Wiseness = Charwise
              | Linewise
              | Blockwise

type Operator = Wiseness -> Mark -> Mark -> Eden World ()

unsafeWithCurBuffer :: Eden Buffer a -> Eden World a
unsafeWithCurBuffer = maybeWithCurBuffer $ error "no current buffer"


runOperator :: Operator -> Motion -> Eden World ()
runOperator op m = do
    (here, there) <- unsafeWithCurBuffer $ do
        here <- getMark
        m
        there <- getMark
        jumpToMark here
        return (here, there)
    op Charwise here there

deleteOp :: Operator
deleteOp w b e = withCurBuffer $ do
    jumpToMark e
    prevChar
    case w of
      Charwise  -> do charwiseTowards delChar b
                      delChar
      otherwise -> error "only charwise is supported"

changeOp :: Operator
changeOp w b e = do
    -- TODO(sandy): there is a bug here for `cw` stealing a trailing space
    deleteOp w b e
    proclaim wMode INSERT
