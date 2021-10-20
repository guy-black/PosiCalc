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
  , _calcState_quoteAct :: Bool
  , _calcState_npSet    :: Maybe (Text, Text)
  , _calcState_npBcksp  :: Bool
  , _calcState_npLeft   :: Bool
  , _calcState_npRight  :: Bool
  } deriving (Show)

data Button
  = ButtonOp Op
  | ButtonEq
  | ButtonClear
  | ButtonBcksp
  | ButtonLeft
  | ButtonRight

initCalcState :: CalcState
initCalcState = CalcState Nothing Nothing False Nothing False False False

updateCalcState :: CalcState -> ((Text, Text), Button) -> CalcState
updateCalcState state@(CalcState acc mOp qa nps npb npl npr) ((bck, fwd), btn) =
  case btn of
    ButtonOp pushedOp -> applyOp (bck<>fwd) state (Just pushedOp)
    ButtonEq -> applyOp (bck<>fwd) state Nothing
    ButtonClear -> initCalcState {_calcState_npSet = (Just ("", ""))}
    ButtonBcksp ->
      if (T.length (bck<>fwd) > 0)
      then state {_calcState_npBcksp = True}
      else case mOp of
        Just _ -> CalcState acc Nothing False Nothing False False False
        Nothing -> case acc of
          Nothing -> initCalcState
          Just ac -> initCalcState {_calcState_npSet = Just ((T.dropEnd 1 (showText ac)), "")}
    ButtonLeft -> CalcState acc mOp False Nothing False True False
    ButtonRight -> CalcState acc mOp False Nothing False False True

applyOp :: Text -> CalcState -> Maybe Op -> CalcState
applyOp input state@(CalcState acc mOp qa nps npb npl npr) mOp' =
  if T.null input
  then
    CalcState acc mOp' False Nothing False False False
  else
    case readMaybe (unpack input) of
      Nothing -> CalcState acc mOp False Nothing False False False -- should only happen if input in numpad ends or stars with . or has a -., do nothing
      Just x -> case mOp of
        Nothing -> CalcState (Just x) mOp' False (Just ("", "")) False False False -- need to clear numpad here
        Just op ->
          -- only in this case do I want to get a new quote
          case acc of
            Nothing -> CalcState (Just (runOp op 0 x)) mOp' True (Just ("","")) False False False -- need to clear numpad here
            Just a -> CalcState (Just (runOp op a x)) mOp' True (Just ("","")) False False False -- need to clear numpad here


mop2Text :: Maybe Op -> Text
mop2Text mOp =
  case mOp of
    Nothing -> " "
    Just Plus -> " + "
    Just Minus -> " - "
    Just Times -> " * "
    Just Divide -> " / "
    Just Percent -> " % "

displayCalcState :: AppWidget js t m => (CalcState, (Text, Text)) -> m()
displayCalcState ((CalcState acc mOp qa nps npb npl npr), (bck, fwd)) =
  case acc of
    Nothing -> text (" " <> mop2Text mOp <> bck <> "|" <> fwd)
    Just accc -> text (((T.pack . show) accc) <> mop2Text mOp <> bck <> "|" <> fwd)

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
    divClass "output" $ dyn_ $ displayCalcState <$> ffor2 calcState numPad (,)
    divClass "output" $
      prerender_
        (text "")
        (quoteBox (ffilter _calcState_quoteAct (updated calcState)))
    (buttons, numPad) <- divClass "input" $ do
      numPad <- divClass "number-pad" $ do
        numPad <- numberPad (fmapMaybe id (_calcState_npSet <$> (updated calcState))) (ffilter id (_calcState_npBcksp <$> (updated calcState))) never never
        return numPad
      (opButtons, bEq) <- divClass "ops-pad" $ do
        let opState = _calcState_op <$> calcState
        bPlus <- opButton Plus "+" opState
        bMinus <- opButton Minus "-" opState
        bTimes <- opButton Times "*" opState
        bDivide <- opButton Divide "/" opState
        bPercent <- opButton Percent "%" opState
        let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide, bPercent]
        bEq <- buttonClass "primary" "="
        return (opButtons, bEq)
      (bClear, bBcksp) <- divClass "other-pad" $ do
        bClear <- buttonClass "secondary" "C"
        bBcksp <- buttonClass "secondary" "⌫"
--        bLeft <- buttonClass "secondary" "<"
--        bRight <- buttonClass "secondary" ">"
        return (bClear, bBcksp)
      return (leftmost
            [ ButtonOp <$> opButtons
            , ButtonEq <$ bEq
            , ButtonClear <$ bClear
            , ButtonBcksp <$ bBcksp
--            , ButtonLeft <$ bLeft
--            , ButtonRight <$ bRight
            ], numPad)
     -- return buttons
    calcState <- accumDyn updateCalcState initCalcState (attach (current numPad) buttons)
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
