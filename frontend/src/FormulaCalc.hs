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

module FormulaCalc where

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
  if (T.head (T.replace "-." "-0." (remdash (t<>" "))) == '.') then
    ("0" <> (T.replace "-." "-0." (remdash (t<>" ")))) else
    (T.replace "-." "-0." (remdash (t<>" ")))

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
