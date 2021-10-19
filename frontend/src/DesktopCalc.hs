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

module DesktopCalc where

import GHC.Int
--import Database.Id.Class (Id(..))
import qualified Data.Map as Map
import Data.Map (Map, (!?))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Read
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)

import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend

import Common.Api
import Common.Route
--import Common.Schema

data Op = Plus | Minus | Times | Divide | Percent
  deriving (Eq, Ord, Show)

data CalcState = CalcState
  { _calcState_acc      :: Maybe Double  -- accumulator
  , _calcState_op       :: Maybe Op      -- most recently requested operation
  , _calcState_input    :: Text          -- current input
  , _calcState_quoteAct :: Bool
  } deriving (Show)

data Button
  = ButtonNumber Text
  | ButtonOp Op
  | ButtonEq
  | ButtonClear
  | ButtonPosNeg
  | ButtonBcksp

initCalcState :: CalcState
initCalcState = CalcState Nothing Nothing "" False

updateCalcState :: CalcState -> Button -> CalcState
updateCalcState state@(CalcState acc mOp input qa) btn =
  case btn of
    ButtonNumber d ->
      if d == "." && T.find (== '.') input /= Nothing
      then state
      else CalcState acc mOp (input <> d) False
    ButtonOp pushedOp -> applyOp state (Just pushedOp)
    ButtonEq -> applyOp state Nothing
    ButtonClear -> initCalcState
    ButtonBcksp ->
      if (T.length input > 0)
      then CalcState acc mOp (T.init input) False
      else case mOp of
        Just _ -> CalcState acc Nothing "" False
        Nothing -> case acc of
          Nothing -> CalcState Nothing Nothing "" False
          Just ac -> updateCalcState (CalcState Nothing Nothing (T.pack $ show ac) False) ButtonBcksp
    ButtonPosNeg ->
      if (T.isPrefixOf "-" input)
      then CalcState acc mOp (T.drop 1 input) False
      else CalcState acc mOp ("-" <> input) False

applyOp :: CalcState -> Maybe Op -> CalcState
applyOp state@(CalcState acc mOp input qa) mOp' =
  if T.null input
  then
    CalcState acc mOp' input False
  else
    case readMaybe (unpack input) of
      Nothing -> CalcState acc mOp "" False -- should only happen if input cannot be parsed as number so reset input
      Just x -> case mOp of
        Nothing -> CalcState (Just x) mOp' "" False
        Just op ->
          -- only in this case do I want to get a new quote
          case acc of
            Nothing -> CalcState (Just (runOp op 0 x)) mOp' "" True
            Just a -> CalcState (Just (runOp op a x)) mOp' "" True


mop2Text :: Maybe Op -> Text
mop2Text mOp =
  case mOp of
    Nothing -> " "
    Just Plus -> " + "
    Just Minus -> " - "
    Just Times -> " * "
    Just Divide -> " / "
    Just Percent -> " % "

displayCalcState :: CalcState -> Text
displayCalcState (CalcState acc mOp input qa) =
  case acc of
    Nothing -> " " <> mop2Text mOp <> input
    Just accc -> ((T.pack . show) accc) <> mop2Text mOp <> input

runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  Divide -> (/)
  Percent -> (\x y -> y/100*x)

desktopCalc :: forall js t m. (AppWidget js t m, Prerender js t m, PerformEvent t m, TriggerEvent t m, DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
desktopCalc = divClass "calculator" $ do
  rec
    divClass "output" $ dynText $ displayCalcState <$> calcState
    divClass "output" $
      prerender_
        (text "")
        (quoteBox (ffilter _calcState_quoteAct (updated calcState)))
    buttons <- divClass "input" $ do
      (numberButtons, bPeriod) <- divClass "number-pad" $ do
        numberButtons <- numberPad
        bPeriod <- ("." <$) <$> buttonClass "number" "."
        return (numberButtons, bPeriod)
      (opButtons, bEq) <- divClass "ops-pad" $ do
        let opState = _calcState_op <$> calcState
        bPlus <- opButton Plus "+" opState
        bMinus <- opButton Minus "-" opState
        bTimes <- opButton Times "*" opState
        bDivide <- opButton Divide "/" opState
        let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
        bEq <- buttonClass "primary" "="
        return (opButtons, bEq)
      (bClear, bBcksp, bPosNeg) <- divClass "other-pad" $ do
        bClear <- buttonClass "secondary" "C"
        bBcksp <- buttonClass "secondary" "⌫"
        bPosNeg <- buttonClass "secondary" "+/-"
        return (bClear, bBcksp, bPosNeg)
      return (leftmost
            [ ButtonNumber <$> numberButtons
            , ButtonNumber <$> bPeriod
            , ButtonOp <$> opButtons
            , ButtonEq <$ bEq
            , ButtonClear <$ bClear
            , ButtonPosNeg <$ bPosNeg
            , ButtonBcksp <$ bBcksp
            ])
     -- return buttons
    calcState <- accumDyn updateCalcState initCalcState buttons
  return ()
  where
    opButton :: Op -> Text -> Dynamic t (Maybe Op) -> m (Event t Op)
    opButton op label selectedOp = do
      (e, _) <- elDynAttr' "button" (("class" =: "primary" <>) <$> (pickColor <$> selectedOp)) $ text label
      return (op <$ domEvent Click e)
      where
        pickColor mOp =
          if Just op == mOp
          then Map.empty
            -- old style "style" =: "background: lightblue"
          else Map.empty
