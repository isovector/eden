module Eden.Operators where

import Eden.Marks
import Eden.Modes.Normal
import Eden.Motions
import Eden.Types
import Eden.Utils

data Wiseness = Charwise
              | Linewise
              | Blockwise

type TextObj = (Mark, Mark)
type Operator = Wiseness -> TextObj -> Eden World ()

unsafeWithCurBuffer :: Eden Buffer a -> Eden World a
unsafeWithCurBuffer = maybeWithCurBuffer $ error "no current buffer"

liftMotion :: Motion -> Eden World TextObj
liftMotion m = do
    let emptyMark = Mark 0 0
        empty = (emptyMark, emptyMark)
    maybeWithCurBuffer empty $ do
        here <- getMark
        m
        there <- getMark
        jumpToMark here
        return (min here there, max here there)

runOperator :: Operator -> TextObj -> Eden World ()
runOperator op tobj = op Charwise tobj

deleteOp :: Operator
deleteOp w (b, e) = withCurBuffer $ do
    jumpToMark e
    prevChar
    case w of
      Charwise  -> do charwiseTowards delChar b
                      delChar
      otherwise -> error "only charwise is supported"

changeOp :: Operator
changeOp w tobj = do
    -- TODO(sandy): there is a bug here for `cw` stealing a trailing space
    deleteOp w tobj
    proclaim wMode INSERT
