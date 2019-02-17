{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- |

module CodeParty.Model where

import CodeParty.Types
import Data.Text (Text)
import Data.Time
import Database.Persist.Sqlite as Persistent
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Editor
    room Room
    uuid SessionId
    UniqueRoomUuid room uuid
    created UTCTime
    activity UTCTime
    title Text
    input Text
    selection Selection
    output Text
    deriving Eq
    deriving Show
 |]
