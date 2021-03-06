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

module Common.Api
  ( AppWidget
  , WidgetWithJS
  , showText
  , readText
  , fromMaybe
  , radioButton
  , numberInput
  , buttonClass
  , numberPad
  , quoteBox
  ) where

import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Read

import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend

import Common.Route

-- common type constraints --
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

-- helper functions that probably already exist in some library i already imported
-- but i was too lazy too look it up so I just made it real quick
showText :: Show a => a -> Text
showText = pack . show
readText :: Read a => Text -> a
readText = read . unpack
fromMaybe :: a -> Maybe a -> a
fromMaybe de m = case m of
  Nothing -> de
  Just j -> j

-- dom widgets
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

buttonClass :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  return $ domEvent Click e

data NumPadChgs = Num Char
                | Mod Char
                | Bcksp
                | SetVal (Text, Text)
                | CurLeft
                | CurRight

numberPad :: AppWidget js t m => Event t (Text, Text) -> Event t a -> Event t b -> Event t c ->  m (Dynamic t (Text, Text))
numberPad setValEv bckspEv leftEv rightEv = do
  b7 <- ((Num '7') <$) <$> buttonClass "number" "7"
  b8 <- ((Num '8') <$) <$> buttonClass "number" "8"
  b9 <- ((Num '9') <$) <$> buttonClass "number" "9"
  b4 <- ((Num '4') <$) <$> buttonClass "number" "4"
  b5 <- ((Num '5') <$) <$> buttonClass "number" "5"
  b6 <- ((Num '6') <$) <$> buttonClass "number" "6"
  b1 <- ((Num '1') <$) <$> buttonClass "number" "1"
  b2 <- ((Num '2') <$) <$> buttonClass "number" "2"
  b3 <- ((Num '3') <$) <$> buttonClass "number" "3"
  b0 <- ((Num '0') <$) <$> buttonClass "number" "0"
  bDot <- ((Mod '.') <$) <$> buttonClass "number" "."
  bInv <- ((Mod 'p') <$) <$> buttonClass "number" "+/-"
  foldDyn updateNumPadVal ("", "") $ leftmost [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, bDot, bInv, (Bcksp <$ bckspEv), (SetVal <$> setValEv), (CurLeft <$ leftEv), (CurRight <$ rightEv)]

updateNumPadVal :: NumPadChgs -> (Text, Text) -> (Text, Text)
updateNumPadVal chg (bck, fwd) =
 case chg of
   Num d -> (bck <> (T.singleton d), fwd)
   Mod m ->
     if m == '.' && T.find (== '.') bck == Nothing && T.find (== '.') fwd == Nothing
     then (bck <> ".", fwd)
     else if m == 'p' && T.find (== '-') bck == Nothing && T.find (== '-') fwd == Nothing
     then ("-" <> bck, fwd)
     else if m == 'p' && T.head bck == '-'
     then (T.tail bck, fwd)
     else (bck, fwd)
   Bcksp -> (T.dropEnd 1 bck, fwd)
   SetVal (t, u) ->
     case verify (t<>u) of
       Nothing -> (bck, fwd)
       Just tt -> (t, u)
   CurLeft ->
     if bck == ""
     then (bck, fwd)
     else (T.dropEnd 1 bck, (T.takeEnd 1 bck)<>fwd)
   CurRight ->
     if fwd == ""
     then (bck, fwd)
     else (bck<>(T.take 1 fwd), (T.drop 1 fwd))

verify :: Text -> Maybe Text
verify t =
  case (readMaybe (unpack t)::Maybe Float) of
    Just tt -> Just t
    Nothing ->
      if t == "" -- handle empty text now to avoid head exceptions
      then Just ""
      else if t == "-" || t == "." -- check for things that would fail readMaybe but could still be a number in the making
      then Just t -- allow just '-' or just '.'
      else if T.head t == '.' && (readMaybe ("0"++(unpack t))::Maybe Float) /= Nothing -- if text starts with . but passes otherwise
      then Just t
      else if T.last t == '.' && (readMaybe ((unpack t)++"0")::Maybe Float) /= Nothing -- if text ends with . but passes otherwise
      then Just t
      else if (readMaybe (unpack (T.replace "-." "-0." t))::Maybe Float) /= Nothing -- if it passes with -. changed to -0.
      then Just t
      else Nothing

-- quote getting thing
quoteRoute :: Text
quoteRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_GetQuote :/ ()

happQuote :: AppWidget js t m => Event t Text -> m (Dynamic t Text)
happQuote ev = holdDyn "" ev

quoteBox :: WidgetWithJS js t m => Event t a -> m (Dynamic t Text)
quoteBox ev = do
  evQuo <- getAndDecode (tag (constant quoteRoute) ev)
  happQuote (fromMaybe "whoops" <$> evQuo) --
