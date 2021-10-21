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

import LongFormCalc
import FormulaCalc
import DesktopCalc
import GraphCalc

header :: AppWidget js t m => m ()
header = divClass "header" $ do
  elAttr "a" ("href" =: "/") $ text "Calculator"
  elAttr "a" ("href" =: "/formula") $ text "Conversions"
  elAttr "a" ("href" =: "/longform") $ text "Equations"

app :: (AppWidget js t m, SetRoute t (R FrontendRoute) m) => RoutedT t (R FrontendRoute) m ()
app =
  subRoute_ $ \case
    FrontendRoute_Main ->
      desktopCalc
    FrontendRoute_Formulas ->
      formulaCalc
    FrontendRoute_LongForm ->
      longFormCalc
    FrontendRoute_Graphing ->
      graphingCalc

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
