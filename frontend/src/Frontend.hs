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

module Frontend where

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

type AppWidget js t m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PerformEvent t m
  , PostBuild t m
  , Prerender js t m
  , TriggerEvent t m
  )

type WidgetWithJS js t m =
  ( AppWidget js t m
  , HasJSContext (Performable m)
  , MonadJSM (Performable m)
  , MonadJSM m
  )

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

buttonClass :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  return $ domEvent Click e

numberPad :: (DomBuilder t m) => m (Event t Text)
numberPad = do
  b7 <- ("7" <$) <$> numberButton "7"
  b8 <- ("8" <$) <$> numberButton "8"
  b9 <- ("9" <$) <$> numberButton "9"
  b4 <- ("4" <$) <$> numberButton "4"
  b5 <- ("5" <$) <$> numberButton "5"
  b6 <- ("6" <$) <$> numberButton "6"
  b1 <- ("1" <$) <$> numberButton "1"
  b2 <- ("2" <$) <$> numberButton "2"
  b3 <- ("3" <$) <$> numberButton "3"
  b0 <- ("0" <$) <$> buttonClass "number zero" "0"
  return $ leftmost [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]
  where
    numberButton n = buttonClass "number" n


runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  Divide -> (/)
  Percent -> (\x y -> y/100*x)


quoteRoute :: Text
quoteRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_GetQuote :/ ()

fromMaybe :: a -> Maybe a -> a
fromMaybe de m = case m of
  Nothing -> de
  Just j -> j

happQuote :: AppWidget js t m => Event t Text -> m (Dynamic t Text)
happQuote ev = holdDyn "" ev

quoteBox :: WidgetWithJS js t m => Event t CalcState -> m ()
quoteBox ev = do
  evQuo <- getAndDecode (tag (constant quoteRoute) ev)
  currQuo <- happQuote (fromMaybe "whoops" <$> evQuo)
  dynText $ currQuo

-- add ops here too
--ops :: Map Op Text
--ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/"), (Percent, "%")]



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

radioButton :: AppWidget js t m => Bool -> Text -> Text -> m (InputElement EventResult (DomBuilderSpace m) t)
radioButton isDefault group label = do
  rec
    retEl <- inputElement $
      (InputElementConfig "" Nothing isDefault Nothing
       (def {_elementConfig_initialAttributes =
             ((AttributeName Nothing "type") =: "radio" <>
              (AttributeName Nothing "id") =: label <>
              (AttributeName Nothing "name") =: group <>
              (AttributeName Nothing "value") =: label)}))
    elAttr "label" ("for" =: label) $ text label
  return retEl

numberInput :: AppWidget js t m => m (InputElement EventResult (DomBuilderSpace m) t)
numberInput =
  inputElement $
    (InputElementConfig "" Nothing False Nothing
     (def {_elementConfig_initialAttributes = ((AttributeName Nothing "type") =: "number" <> (AttributeName Nothing "Value") =: "0")}))

data ConvType = Dist | Vol | Weigh | Temp
  deriving Show

data Unit = MM -- millimeter
          | CM -- centimeter
          | M -- meter
          | KM -- kilometer
          | IN -- inch
          | FT -- feet
          | YD -- yard
          | MI -- mile
          | MN -- m&m
          | MMN -- mini m&m
          | GMN -- giant m&m
          | ML -- milliliter
          | CL -- centiliter
          | L -- liter
          | TSP -- teaspoon
          | TBSP -- tablespoon
          | FLOZ -- fluid ounce
          | CP -- cup
          | PT -- pint
          | QT -- quart
          | GL -- Gallon
          | MG -- milligram
          | G -- gram
          | KG -- kilogram
          | OZ -- ounce
          | LB -- pound
          | TN -- ton
          | F -- farenheit
          | C -- celsius
          | K -- kelvin
          | NONE -- this left blank
  deriving (Eq, Ord, Show)

convScale :: Map Unit (Float, Unit)
convScale = ( MM =: (1, MM) <>
              CM =: (10, MM) <>
               M =: (1000, MM) <>
              KM =: (1000000, MM) <>
              IN =: (25.4, MM) <>
              FT =: (304.8, MM) <>
              MI =: (1609344, MM) <>
              MN =: (10.4, MM) <>
             MMN =: (9.525, MM) <>
             GMN =: (21.2, MM) <>
              ML =: (1, ML) <>
              CL =: (10, ML) <>
               L =: (1000, ML) <>
             TSP =: (4.92892, ML) <>
            TBSP =: (14.78676, ML) <>
            FLOZ =: (29.57352, ML) <>
              CP =: (236.58816, ML) <>
              PT =: (473.17632, ML) <>
              QT =: (946.35264, ML) <>
              GL =: (3785.41056, ML) <>
              MG =: (1, MG) <>
               G =: (1000, MG) <>
              KG =: (1000000, MG) <>
              OZ =: (28349.523125, MG) <>
              LB =: (453592.37, MG) <>
              TN =: (907184740, MG))



convMap :: ConvType -> Map Unit Text
convMap c = case c of
  Dist -> ( MM =: "MM" <>
            CM =: "CM" <>
             M =: "Meter"  <>
            KM =: "KM" <>
            IN =: "Inch" <>
            FT =: "Feet" <>
            YD =: "Yard" <>
            MI =: "Mile" <>
            MN =: "M&M" <>
           MMN =: "Mini M&M" <>
           GMN =: "Giant M&M")

  Vol -> (  ML =: "Ml" <>
            CL =: "Cl" <>
             L =: "Liter" <>
           TSP =: "Tsp" <>
          TBSP =: "Tbsp" <>
          FLOZ =: "Fl.oz" <>
            CP =: "Cup" <>
            PT =: "Pint" <>
            QT =: "quart" <>
            GL =: "gallon")

  Weigh -> (MG =: "MG" <>
             G =: "gram" <>
            KG =: "Kg" <>
            OZ =: "ounce" <>
            LB =: "pound" <>
            TN =: "ton")

  Temp -> (  F =: "°F" <>
             C =: "°C" <>
             K =: "°K")

convPicker :: AppWidget js t m => m (Dynamic t ConvType)
convPicker = do
  distButton <- radioButton True "conv" "Distance"
  volButton <- radioButton False "conv" "Volume"
  weiButton <- radioButton False "conv" "Weight"
  tmpButton <- radioButton False "conv" "Temperature"
  holdDyn Dist (leftmost
               [ Dist <$ (((ffilter id) . _inputElement_checkedChange) $ distButton)
               , Vol <$ (((ffilter id) . _inputElement_checkedChange) $ volButton)
               , Weigh <$ (((ffilter id) . _inputElement_checkedChange) $ weiButton)
               , Temp <$ (((ffilter id) . _inputElement_checkedChange) $ tmpButton)
               ])

convert :: Text -> Unit -> Unit -> Text
convert num from to =
  case (readMaybe(unpack(fixup (num)))::Maybe Float) of
    Nothing -> ""
    Just n ->
      -- checking if a conversion even needs to happen
      if (from == to) then
        num
      else
        case (from, to) of
          (NONE, NONE) -> ""
          (_, NONE) -> ""
          (NONE, _) -> ""
          -- manually deal with temp conversions
          (F, C) -> showText (((n-32)*5)/9)
          (F, K) -> showText ((((n-32)*5)/9)+273.15)
          (K, F) -> showText ((n-273.15)*1.8+32)
          (K, C) -> showText (n-273.15)
          (C, K) -> showText (n+273.15)
          (C, F) -> showText (n*1.8+32)
          -- handle conversions to or from their smallest unit
          (MM, _) ->
            case (convScale !? to) of
              Nothing -> case to of
                NONE -> ""
                _ -> ("error, " <> (showText to) <> " not found in scale map")
              Just (f, _) -> showText (n/f)
          (ML, _) ->
            case (convScale !? to) of
              Nothing -> case to of
                NONE -> ""
                _ -> ("error, " <> (showText to) <> " not found in scale map")
              Just (f, _) -> showText (n/f)
          (MG, _) ->
            case (convScale !? to) of
              Nothing -> case to of
                NONE -> ""
                _ -> ("error, " <> (showText to) <> " not found in scale map")
              Just (f, _) -> showText (n/f)
          (_, MM) ->
            case (convScale !? from) of
              Nothing -> case from of
                NONE -> ""
                _ -> ("error, " <> (showText from) <> " not found in scale map")
              Just (f, _) -> showText (n*f)
          (_, ML) ->
            case (convScale !? from) of
              Nothing -> case from of
                NONE -> ""
                _ -> ("error, " <> (showText from) <> " not found in scale map")
              Just (f, _) -> showText (n*f)
          (_, MG) ->
            case (convScale !? from) of
              Nothing -> case from of
                NONE -> ""
                _ -> ("error, " <> (showText from) <> " not found in scale map")
              Just (f, _) -> showText (n*f)
          -- conversions neither to nor from their smallest unit
          (_, _) ->
            case (convScale !? from) of
              Nothing -> case from of
                NONE -> ""
                _ -> ("error, " <> (showText from) <> " not found in scale map")
              Just (f, u) -> convert (showText(n*f)) u to

--   remove any '-' after the first char
remdash :: Text -> Text
remdash t = (T.singleton(T.head t)) <> (T.filter (/='-') (T.tail t))

fixup :: Text -> Text
fixup t =
  --
  if (T.head (T.replace "-." "-0." (remdash (t<>" "))) == '.') then
    ("0" <> (T.replace "-." "-0." (remdash (t<>" ")))) else
    (T.replace "-." "-0." (remdash (t<>" ")))

showText :: Show a => a -> Text
showText = pack . show

        

formulaCalc :: AppWidget js t m => m ()
formulaCalc =
  divClass "convType" $ do
    dynConvType <- convPicker
    numInput <- numberInput
    fromUnit <- dropdown NONE (convMap <$> dynConvType) (DropdownConfig never (constDyn Map.empty))
    rec
      display (ffor3 (_inputElement_value numInput) (_dropdown_value fromUnit) (_dropdown_value toUnit) convert)
      toUnit <- dropdown NONE (convMap <$> dynConvType) (DropdownConfig never (constDyn Map.empty))
    return ()

longFormCalc :: AppWidget js t m => m ()
longFormCalc = text "Long form calculator"

header :: AppWidget js t m => m ()
header = divClass "header" $ do
  elAttr "a" ("href" =: "/" <> "class" =: "number") $ text "Simple Calculator"
  elAttr "a" ("href" =: "/formula") $ text "Formulas and Conversions"
  elAttr "a" ("href" =: "/longform") $ text "Longform Calculator"

app :: (AppWidget js t m, SetRoute t (R FrontendRoute) m) => RoutedT t (R FrontendRoute) m ()
app =
  subRoute_ $ \case
    FrontendRoute_Main ->
      desktopCalc
    FrontendRoute_Formulas ->
      formulaCalc
    FrontendRoute_LongForm ->
      longFormCalc

viewport :: Map Text Text
viewport = "name" =: "viewport"
        <> "content" =: "width=device-width"

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "posicalc ^-^ <3"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "meta" viewport blank
  , _frontend_body = do
      header
      app
  }
