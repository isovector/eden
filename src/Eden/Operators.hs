{-# LANGUAGE LambdaCase #-}

module Eden.Operators where

import Eden.Marks
import Eden.Modes.Normal
import Eden.Motions
import Eden.TextObjs
import Eden.Types
import Eden.Utils

import Control.Monad (when)

import qualified Data.List.Zipper as Z


type Operator = TextObj -> Eden World ()

unsafeWithCurBuffer :: Eden Buffer a -> Eden World a
unsafeWithCurBuffer = maybeWithCurBuffer $ error "no current buffer"

operateToEnd :: Operator -> Repeatable World ()
operateToEnd op = do
    runOperator op =<< lift (liftCharwise jumpEnd)
    lift $ inquire wMode >>= \case
        NORMAL -> withCurBuffer sanitizeCursor
        INSERT -> return ()

runOperator :: Operator -> TextObj -> Repeatable World ()
runOperator op tobj@(TextObj w b e) = lift $
    when (b /= e) $ op tobj

operator :: Operator -> Repeatable World ()
operator op = do
    getTextObj >>= \case
        Just m  -> lift m >>= runOperator op
        Nothing -> return ()

deleteOp :: Operator
deleteOp (TextObj w b e) = withCurBuffer $ do
    jumpToMark e
    prevChar
    case w of
      -- TODO(sandy): this won't delete newlines in charwise mode
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
