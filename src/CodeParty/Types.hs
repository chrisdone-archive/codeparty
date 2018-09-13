{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- |

module CodeParty.Types where

import           Control.Concurrent.STM
import           Data.Pool
import           Data.Text (Text)
import           Database.Persist.Sqlite as Persistent
import           Yesod
import           Yesod.EmbeddedStatic

data SiteManager = SiteManager
  { manUserName :: Text
  , manPassWord :: Text
  } deriving (Show, Eq)

data App = App
  { appPool :: Pool SqlBackend
  , appRoot :: Text
  , appSiteManager :: SiteManager
  , appStatic :: EmbeddedStatic
  , appChans :: TChan Room
  }

newtype Room = Room {unRoom :: Text}
  deriving (PersistFieldSql, PersistField, Show, Read, Eq, PathPiece, FromJSON)

newtype SessionId = SessionId {unSessionId :: Text}
  deriving (PersistFieldSql, PersistField, Show, Read, Eq, PathPiece, FromJSON, ToJSON)
