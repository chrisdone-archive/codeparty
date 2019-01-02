{-# LANGUAGE ScopedTypeVariables #-}
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

module CodeParty where
import           Data.Monoid
import           CodeParty.Foundation
import           CodeParty.Model
import           CodeParty.Types
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import           Data.List (find)
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Time
import           Data.Void
import           Database.Persist.Sql
import           Lucid
import           Yesod hiding (toHtml)
import           Yesod.Lucid
import           Yesod.WebSockets

data Eupdate = Eupdate
  { eupdateTitle :: Text
  , eupdateInput :: Text
  , eupdateSelection :: Selection
  }

instance FromJSON Eupdate where
  parseJSON j = do
    o <- parseJSON j
    Eupdate <$> o.: "title" <*> o.: "input" <*> o.: "selection"

mkYesodDispatch "App" resourcesApp

getRoomR :: Room -> Handler LucidHtml
getRoomR roomid = do
  webSockets (interaction roomid)
  lucid
    (\url -> do
       title_ (toHtml (unRoom roomid))
       sequence_
         [ link_ [rel_ "stylesheet", type_ "text/css", href_ (url (StaticR route))]
         | route <- [codemirror_css, zenburn_css, index_css]
         ]
       sequence_
         [ script_ [src_ (url (StaticR route))] ("" :: Text)
         | route <- [codemirror_js, haskell_js, index_js]
         ])

interaction :: Room -> WebSocketsT Handler ()
interaction roomId = do
  sessionId <- getSessionId
  broadcaster <- fmap appChans getYesod
  updates <- liftIO (atomically (dupTChan broadcaster))
  lift (createEditorIfMissing roomId sessionId)
  race_ (receiveLoop roomId sessionId) (sendLoop updates roomId)

receiveLoop :: Room -> SessionId -> WebSocketsT Handler Void
receiveLoop roomId sessionId = do
  forever
    (do mstr <- receiveDataE
        case mstr of
          Left _ ->
            lift
              (do runDB
                    (updateWhere
                       [EditorUuid ==. sessionId]
                       [EditorConnected =. False])
                  signalUpdated roomId)
          Right str ->
            case decode str of
              Just eupdate ->
                lift
                  (do runDB
                        (updateWhere
                           [EditorUuid ==. sessionId]
                           [ EditorTitle =. eupdateTitle eupdate
                           , EditorInput =. eupdateInput eupdate
                           , EditorSelection =. eupdateSelection eupdate
                           ])
                      signalUpdated roomId)
              Nothing -> error ("Invalid incoming update!" <> show str))

sendLoop :: TChan Room -> Room -> WebSocketsT Handler Void
sendLoop updates roomId = do
  forever
    (do room <- liftIO (atomically (readTChan updates))
        when
          (room == roomId)
          (do editors <- lift (runDB (getEditors roomId))
              sendEditors (map entityVal editors)))

sendEditors :: [Editor] -> WebSocketsT Handler ()
sendEditors editors =
  sendTextData
    (encode
       (map
          (\editor ->
             object
               [ "session" .= editorUuid editor
               , "title" .= editorTitle editor
               , "input" .= editorInput editor
               , "output" .= editorOutput editor
               , "selection" .= editorSelection editor
               ])
          editors))

signalUpdated :: (HandlerSite m ~ App, MonadHandler m) => Room -> m ()
signalUpdated roomId = do
  updates <- fmap appChans getYesod
  liftIO (atomically (writeTChan updates roomId))

getSessionId :: WebSocketsT Handler SessionId
getSessionId = do
  str :: LT.Text <- receiveData
  case decode (LT.encodeUtf8 str) of
    Nothing -> error "Invalid sessionid"
    Just s -> pure s

createEditorIfMissing :: Room -> SessionId -> Handler ()
createEditorIfMissing roomid sessionId = do
  runDB
    (do now <- liftIO getCurrentTime
        updateWhere [EditorUuid ==. sessionId] [EditorConnected =. True]
        editors <- getEditors roomid
        case find (\(Entity _ editor) -> editorUuid editor == sessionId) editors of
          Nothing ->
            void
              (insert
                 Editor
                   { editorSelection =
                       Selection
                         { selectionStartLine = 1
                         , selectionStartCh = 1
                         , selectionEndLine = 1
                         , selectionEndCh = 1
                         }
                   , editorRoom = roomid
                   , editorUuid = sessionId
                   , editorCreated = now
                   , editorEdited = now
                   , editorTitle = ""
                   , editorInput = ""
                   , editorOutput = ""
                   , editorConnected = True
                   })
          Just {} -> pure ())
  signalUpdated roomid

getEditors ::
     (BaseBackend backend ~ SqlBackend, MonadIO m, PersistQueryRead backend)
  => Room
  -> ReaderT backend m [Entity Editor]
getEditors roomid =
  selectList
    [EditorRoom ==. roomid, EditorConnected ==. True]
    [Asc EditorCreated]
