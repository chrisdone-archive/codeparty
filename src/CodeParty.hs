{-# LANGUAGE DeriveFunctor #-}
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
import qualified Data.Text.Encoding as T
import           Data.Time
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
import           Data.Void
import           Database.Persist.Sql
import           Lucid
import           Yesod hiding (toHtml)
import           Yesod.Lucid
import           Yesod.WebSockets

data Eupdate = Eupdate
  { eupdateTitle :: Text
  , eupdateInput :: Text
  , eupdateSelection :: Maybe Selection
  }

instance FromJSON Eupdate where
  parseJSON j = do
    o <- parseJSON j
    Eupdate <$> o.: "title" <*> o.: "input" <*> o.:? "selection"

mkYesodDispatch "App" resourcesApp

postRoomR :: Room -> Handler ()
postRoomR roomid = do
  eupdate <- requireJsonBody
  msessionId <- lookupHeader "Authorization"
  case msessionId of
    Nothing -> error "Need Authorization header."
    Just sessionId ->
      do let sessId = (SessionId (T.decodeUtf8 sessionId))
         _ <- createEditorIfMissing roomid (FromPostRequest sessId)
         handleEUpdate
           roomid
           sessId
           (FromPostRequest eupdate)

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
  e <- lift (createEditorIfMissing roomId (FromWebSocket sessionId))
  initializeEditor e
  race_ (receiveLoop roomId sessionId) (sendLoop sessionId updates roomId)

receiveLoop :: Room -> SessionId -> WebSocketsT Handler Void
receiveLoop roomId sessionId = do
  forever
    (do mstr <- receiveDataE
        case mstr of
          Left _ -> pure ()
          Right str ->
            case decode str of
              Just eupdate -> lift (handleEUpdate roomId sessionId (FromWebSocket eupdate))
              Nothing -> error ("Invalid incoming update!" <> show str))

handleEUpdate :: Room -> SessionId -> Originated Eupdate -> HandlerFor App ()
handleEUpdate roomId sessionId originated = do
  now <- liftIO getCurrentTime
  runDB
    (updateWhere
       [EditorUuid ==. sessionId]
       ([ EditorTitle =. eupdateTitle eupdate
        , EditorInput =. eupdateInput eupdate
        , EditorActivity =. now
        ] ++
        [ EditorSelection =. selection
        | Just selection <- [eupdateSelection eupdate]
        ]))
  signalUpdated (fmap (const roomId) originated)
  where eupdate = unOriginated originated

sendLoop ::
     SessionId -> TChan (Originated Room) -> Room -> WebSocketsT Handler Void
sendLoop sessionId updates roomId = do
  forever
    (do room <- liftIO (atomically (readTChan updates))
        when
          (unOriginated room == roomId)
          (do now <- liftIO getCurrentTime
              editors <- lift (runDB (getEditors roomId))
              sendEditors
                (filter
                   (\e -> editorConnected e now)
                   (filter
                      (\e ->
                         case room of
                           FromWebSocket {} -> editorUuid e /= sessionId
                           FromPostRequest {} -> True)
                      (map entityVal editors)))))

initializeEditor :: Editor -> WebSocketsT Handler ()
initializeEditor editor =
  do now <- liftIO getCurrentTime
     sendTextData
       (encode
          (object
             [ "tag" .= ("InitializeEditor" :: Text)
             , "editor" .=
               object
                 [ "session" .= editorUuid editor
                 , "title" .= editorTitle editor
                 , "input" .= editorInput editor
                 , "output" .= editorOutput editor
                 , "selection" .= editorSelection editor
                 , "connected" .= editorConnected editor now
                 ]
             ]))

sendEditors :: [Editor] -> WebSocketsT Handler ()
sendEditors editors =
  do now <- liftIO getCurrentTime
     sendTextData
       (encode
          (object
             [ "tag" .= ("IncomingViewersUpdate" :: Text)
             , "viewers" .=
               (map
                  (\editor ->
                     object
                       [ "session" .= editorUuid editor
                       , "title" .= editorTitle editor
                       , "input" .= editorInput editor
                       , "output" .= editorOutput editor
                       , "selection" .= editorSelection editor
                       , "connected" .= editorConnected editor now
                       ])
                  editors)
             ]))

editorConnected :: Editor -> UTCTime -> Bool
editorConnected editor now = diffUTCTime now (editorActivity editor) < 60 * 30

signalUpdated :: (HandlerSite m ~ App, MonadHandler m) => Originated Room -> m ()
signalUpdated roomId = do
  updates <- fmap appChans getYesod
  liftIO (atomically (writeTChan updates roomId))

getSessionId :: WebSocketsT Handler SessionId
getSessionId = do
  str :: LT.Text <- receiveData
  case decode (LT.encodeUtf8 str) of
    Nothing -> error "Invalid sessionid"
    Just s -> pure s

createEditorIfMissing :: Room -> Originated SessionId -> Handler Editor
createEditorIfMissing roomid osessionId = do
  e <-
    runDB
      (do now <- liftIO getCurrentTime
          updateWhere [EditorUuid ==. sessionId] [EditorActivity =. now]
          editors <- getEditors roomid
          case find
                 (\(Entity _ editor) -> editorUuid editor == sessionId)
                 editors of
            Nothing -> do
              void (insert e)
              pure e
              where e =
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
                        , editorActivity = now
                        , editorTitle = ""
                        , editorInput = ""
                        , editorOutput = ""
                        }
            Just e -> pure (entityVal e))
  signalUpdated (fmap (const roomid) osessionId)
  pure e
  where sessionId = unOriginated osessionId

getEditors ::
     (BaseBackend backend ~ SqlBackend, MonadIO m, PersistQueryRead backend)
  => Room
  -> ReaderT backend m [Entity Editor]
getEditors roomid =
  selectList
    [EditorRoom ==. roomid]
    [Asc EditorCreated]
