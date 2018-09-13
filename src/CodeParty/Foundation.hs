{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module CodeParty.Foundation where

import Data.Text (Text)
import Database.Persist.Sqlite as Persistent
import Development
import CodeParty.Types
import Yesod hiding (toHtml, Html)
import Yesod.EmbeddedStatic

mkYesodData "App" [parseRoutes|
  /static StaticR EmbeddedStatic appStatic
  !/#Room RoomR GET
|]

mkEmbeddedStatic development "embeddedStatic" [embedDir "static"]

instance Yesod App where
  addStaticContent = embedStaticContent appStatic StaticR Right
  approot = ApprootMaster appRoot
  maximumContentLength _ _ = Just (1024 * 20)
--
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB act = do
    App {appPool = pool} <- getYesod
    runSqlPool act pool
