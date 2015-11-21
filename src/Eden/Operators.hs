{-# LANGUAGE LambdaCase #-}

module Eden.Operators where

import Eden.Marks
import Eden.Modes.Normal
import Eden.Motions
import Eden.TextObjs
import Eden.Types
import Eden.Utils


data Wiseness = Charwise
              | Linewise
              | Blockwise

type Operator = Wiseness -> TextObj -> Eden World ()

unsafeWithCurBuffer :: Eden Buffer a -> Eden World a
unsafeWithCurBuffer = maybeWithCurBuffer $ error "no current buffer"

runOperator :: Operator -> TextObj -> Eden World ()
runOperator op tobj@(b, e) =
    -- TODO(sandy): this logic is wrong for linewise
    if b /= e
       then op Charwise tobj
       else return ()

operator :: Operator -> Eden World ()
operator op = do
    getTextObj >>= \case
        Just m  -> m >>= runOperator op
        Nothing -> return ()

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
