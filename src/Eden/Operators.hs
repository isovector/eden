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


type Operator = TextObj -> Eden Buffer ()

operateToEnd :: Operator -> Repeatable Buffer ()
operateToEnd op = do
    runOperator op =<< lift (liftCharwise jumpEnd)
    lift . escape $ inquire wMode >>= \case
        NORMAL -> withCurBuffer sanitizeCursor
        INSERT -> return ()

runOperator :: Operator -> TextObj -> Repeatable Buffer ()
runOperator op tobj@(TextObj w b e) = lift $
    when (b /= e) $ op tobj

operator :: Operator -> Repeatable Buffer ()
operator op = do
    getTextObj >>= \case
        Just m  -> lift m >>= runOperator op
        Nothing -> return ()

deleteOp :: Operator
deleteOp (TextObj w b e) = do
    jumpToMark e
    prevChar
    case w of
      -- TODO(sandy): this won't delete newlines in charwise mode
      Charwise -> do charwiseTowards delChar b
                     delChar
      Linewise -> do linewiseTowards (arrests bLines Z.delete) b
                     arrest cursorX 0

changeOp :: Operator
changeOp tobj@(TextObj w _ _) = do
    -- TODO(sandy): there is a bug here for `cw` stealing a trailing space
    deleteOp tobj
    case w of
      Charwise -> return ()
      Linewise -> openLine $ return ()
    escape $ arrest wMode INSERT
