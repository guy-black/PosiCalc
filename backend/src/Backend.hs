{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import qualified Common.Route as R
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.Word (Word64)
import Database.Id.Class (Id(..), unId)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (execute_, Query, query, query_)
import Gargoyle.PostgreSQL.Connect (withDb)
import Obelisk.Backend (Backend(..), _backend_run, _backend_routeEncoder)
import Obelisk.Route (renderBackendRoute, renderFrontendRoute, pattern (:/))
import qualified Snap.Core as S

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS quotes\
  \ (id SERIAL PRIMARY KEY, quote TEXT NOT NULL)"



backend :: Backend R.BackendRoute R.FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      withDb "db" $ \pool -> do
        _ <- withResource pool $ \dbcon -> execute_ dbcon migration
        serve $ \case
          R.BackendRoute_GetQuote :/ () -> do
            result <- liftIO $ withResource pool $ \dbcon ->
              query_ dbcon "SELECT quote FROM quotes ORDER BY RANDOM () LIMIT 1;"
            case (result :: [[Text]]) of
              [[quote]] -> S.writeBS $ toStrict $ A.encode quote
              _ -> S.modifyResponse $ S.setResponseStatus 404 "Not Found"

            --S.writeBS $ toStrict $ A.encode (T.pack "Coming Soon: Positivity")

          _ -> S.redirect $ encodeUtf8 $ renderFrontendRoute R.checkedFullRouteEncoder $
            R.FrontendRoute_Main :/ ()
  , _backend_routeEncoder = R.fullRouteEncoder
  }
