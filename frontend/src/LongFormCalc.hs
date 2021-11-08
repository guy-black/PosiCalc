{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LongFormCalc where

import GHC.Int
--import Database.Id.Class (Id(..))
import qualified Data.Map as Map
import Data.Map (Map, (!?))
import Data.Text (Text)
import Data.List
import qualified Data.Text as T
import Text.Read
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Control.Monad (join)
import Language.Javascript.JSaddle (MonadJSM)

import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend

import Common.Api
import Common.Route
--import Common.Schema

data Segment = Operator Op
             | Num Text
             | NumWCur
             | Paren Bool
             | Curs
  deriving Eq
instance Show Segment where
  show x =
    case x of
      Operator o -> show o
      Num t -> show t
      NumWCur -> "I'll fix this later"
      Paren ls -> "("++(show ls)++")"
      Curs -> "|"

data Deltas = Bcksp
            | CurLeft
            | CurRight
            | Clear
            | NumPress Text
            | Oper Op
            | Pare Bool

data Expression =
  Expression {
    _expression_segments :: [Segment]
  , _expression_solvable :: Bool
  , _expression_npSet :: Maybe (Text, Text)
  , _expression_npLeft :: Bool
  , _expression_npRight :: Bool
  , _expression_npBcksp :: Bool
             }

data Op = Plus
        | Minus
        | Mult
        | Div
        | Exp
  deriving Eq
instance Show Op where
  show x =
    case x of
      Plus -> "+"
      Minus -> "-"
      Mult -> "×"
      Div -> "/"
      Exp -> "^"


-- Bcksp CurLeft and CurRight need to account for moving into a Num
updateExp :: Deltas -> Expression -> Expression
updateExp chg exp@(Expression li b nps npl npr npb) =
  case chg of
    Bcksp ->
      if elem NumWCur li
      then Expression li b Nothing False False True
      else case elemIndex Curs li of
        Nothing -> exp -- shouldn't happen
        Just t ->
          if t==0
          then Expression li b Nothing False False False
          else case li!!(t-1) of
            Num a -> Expression (take (t-1) li <> (NumWCur:[]) <> drop t li) b (Just (T.init a, "")) False False False
            _ -> Expression ((take (t-1) li) <> (drop t li)) b Nothing False False False
    CurLeft ->
      if elem NumWCur li
      then Expression li b Nothing True False False
      else case elemIndex Curs li of
        Nothing -> exp --shouldn't happen
        Just t ->
          if t==0
          then Expression li b Nothing False False False
          else case li!!(t-1) of
            Num a -> Expression ((take (t-1) li) <> (NumWCur:[]) <> (drop t li)) b (Just (a, "")) False False False
            _ ->  Expression ((take (t-1) li) <> ((li!!t):[]) <> ((li!!(t-1)):[]) <> (drop (t+1) li)) b Nothing False False False
    CurRight ->
      if elem NumWCur li
      then Expression li b Nothing True False False
      else case elemIndex Curs li of
        Nothing -> exp --shouldn't happen
        Just t ->
          if t == length li-1
          then Expression li b Nothing False False False
          else case li!!(t+1) of
            Num a -> Expression ((take (t+1) li) <> (NumWCur:[]) <> (drop (t+2) li)) b (Just (T.init "", a)) False False False -- replace this Num with NumWCur, set numpad to ("", a)
            _ -> Expression ((take t li) <> ((li!!(t+1)):[]) <> ((li!!t):[]) <> (drop (t+2) li)) b Nothing False False False
    Clear -> Expression [] False (Just ("","")) False False False
    NumPress t ->
      if elem NumWCur li
      then Expression li b Nothing False False False -- do nothing, the widget already handles it
      else case elemIndex Curs li of
        Nothing -> exp --shouldn't happen
        Just t -> Expression ((take t li) <> (NumWCur:(drop (t+1) li))) b Nothing False False False
    Oper o -> exp
    Pare p -> exp

longFormCalc :: AppWidget js t m => m ()
longFormCalc = do
  rec
    currExpr <- foldDyn updateExp (Expression [Curs] False Nothing False False False) (leftmost [Bcksp <$ bcksp, Clear <$ clr, CurLeft <$ left, CurRight <$ right, (NumPress . (\(f,t)->f<>t)) <$> (updated numPad)])
    -- this looks an awful lot like a Bad Solution™
    numPad <- numberPad (fmapMaybe id (_expression_npSet <$> (updated currExpr))) (ffilter id (_expression_npBcksp <$> (updated currExpr))) (ffilter id (_expression_npLeft <$> (updated currExpr))) (ffilter id (_expression_npRight <$> (updated currExpr)))
    display (_expression_segments <$> currExpr)
    bcksp <- button "⌫"
    clr <- button "clr"
    left <- button "<-"
    right <- button "->"
  return ()
