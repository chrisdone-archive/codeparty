{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- |

module CodeParty.Types where

import           Control.Concurrent.STM
import           Data.Monoid
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
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

data Selection = Selection
  { selectionStartLine :: Int
  , selectionStartCh :: Int
  , selectionEndLine :: Int
  , selectionEndCh :: Int
  } deriving (Show, Read, Eq)

instance PersistFieldSql Selection where
  sqlType _ = SqlString

instance PersistField Selection where
  toPersistValue (Selection sl sc el ec) =
    toPersistValue (unwords (map show [sl, sc, el, ec]))
  fromPersistValue val = do
    str <- fromPersistValue val
    case map read (words str) of
      [sl, sc, el, ec] -> pure (Selection sl sc el ec)
      _ -> Left ("Invalid selection: " <> T.pack str)

instance FromJSON Selection where
  parseJSON j = do
    o <- parseJSON j
    head' <- o .: "head"
    tail' <- o .: "tail"
    selectionStartLine <- head' .: "line"
    selectionStartCh <- head' .: "ch"
    selectionEndLine <- tail' .: "line"
    selectionEndCh <- tail' .: "ch"
    pure Selection {..}

instance ToJSON Selection where
  toJSON (Selection sl sc el ec) =
    object
      [ "head" .= object ["line" .= sl, "ch" .= sc]
      , "tail" .= object ["line" .= el, "ch" .= ec]
      ]
