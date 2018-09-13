{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CodeParty
import           CodeParty.Foundation
import           CodeParty.Model
import           CodeParty.Types
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Data.Monoid
import           Data.Pool
import qualified Data.Text as T
import           Database.Persist.Sqlite as Persistent
import           Options.Applicative.Simple
import           System.Environment
import           Yesod hiding (toHtml, Html)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  ((port, connstr, root, connections), ()) <-
    simpleOptions
      "0.0.0"
      "codeparty-web"
      "Webmail"
      ((,,,) <$>
       option
         auto
         (metavar "PORT" <> help "Port to listen on" <> short 'p' <> long "port") <*>
       strOption
         (metavar "FILEPATH" <> help "SQLite file" <>
          long "sqlite-file") <*>
       strOption
         (metavar "ROOT" <>
          help "App root e.g. https://foo.com (no trailing slash)" <>
          long "approot") <*>
       (option
          auto
          (metavar "COUNT" <> help "Max database connections" <>
           long "max-db-connections" <>
           value 1)))
      empty
  manager <-
    SiteManager <$> pure "" <*> pure ""
  chan <- newBroadcastTChanIO
  runStdoutLoggingT
    (withSqlitePool
       connstr
       connections
       (\pool -> do
          withResource
            pool
            (runReaderT (runMigration CodeParty.Model.migrateAll))
          liftIO (warp port (App pool root manager embeddedStatic chan))))
