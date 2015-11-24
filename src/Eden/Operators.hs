{-# LANGUAGE LambdaCase #-}

module Eden.Operators where

import Eden.Marks
import Eden.Modes.Normal
import Eden.Motions
import Eden.TextObjs
import Eden.Types
import Eden.Utils

import qualified Data.List.Zipper as Z


type Operator = TextObj -> Eden World ()

unsafeWithCurBuffer :: Eden Buffer a -> Eden World a
unsafeWithCurBuffer = maybeWithCurBuffer $ error "no current buffer"

operateToEnd :: Operator -> Eden World ()
operateToEnd op = do
    runOperator op =<< liftCharwise jumpEnd
    inquire wMode >>= \case
        NORMAL -> withCurBuffer sanitizeCursor
        INSERT -> return ()

runOperator :: Operator -> TextObj -> Eden World ()
runOperator op tobj@(TextObj w b e) =
    if b /= e
       then op tobj
       else return ()

operator :: Operator -> Eden World ()
operator op = do
    getTextObj >>= \case
        Just m  -> m >>= runOperator op
        Nothing -> return ()

deleteOp :: Operator
deleteOp (TextObj w b e) = withCurBuffer $ do
    jumpToMark e
    prevChar
    case w of
      Charwise -> do charwiseTowards delChar b
                     delChar
      Linewise -> do linewiseTowards (proclaims bLines Z.delete) b
                     proclaim cursorX 0

changeOp :: Operator
changeOp tobj@(TextObj w _ _) = do
    -- TODO(sandy): there is a bug here for `cw` stealing a trailing space
    deleteOp tobj
    case w of
      Charwise -> return ()
      Linewise -> openLine $ return ()
    proclaim wMode INSERT
