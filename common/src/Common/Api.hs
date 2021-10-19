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

module Common.Api where

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


-- quote getting thing
quoteRoute :: Text
quoteRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_GetQuote :/ ()

happQuote :: AppWidget js t m => Event t Text -> m (Dynamic t Text)
happQuote ev = holdDyn "" ev

quoteBox :: WidgetWithJS js t m => Event t a -> m ()
quoteBox ev = do
  evQuo <- getAndDecode (tag (constant quoteRoute) ev)
  currQuo <- happQuote (fromMaybe "whoops" <$> evQuo)
  dynText $ currQuo
