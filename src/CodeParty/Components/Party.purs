-- | This is where the editors are all brought together.

module CodeParty.Components.Party where

import Data.Either.Nested (Either2)
import CodeParty.Components.Editor as Editor
import CodeParty.Components.Viewer as Viewer
import CodeParty.Types (Editor(..), Room, Selection, SessionId(..), objectLookup)
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array as Array
import Data.Either
import Data.Either (either)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2, cp3, cp4)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import Halogen.Websocket as Websocket
import Prelude

--------------------------------------------------------------------------------
-- Types

data Input = Input
  { sessionId :: SessionId
  , room :: Room
  }

data State = State
  { sessionId :: SessionId
  , room :: Room
  , mwebsocket :: Maybe Websocket.Websocket
  , viewers :: Array Editor
  , editor :: Maybe Editor
  , layout :: Layout
  }

data Query a
  = Initialize a
  | WebsocketError String a
  | WebsocketIncoming WebsocketIncoming a
  | OutgoingEditorUpdate Editor a
  | SetLayout Layout a

data WebsocketIncoming
  = InitializeEditor Editor
  | IncomingViewersUpdate (Array Editor)
  | Unknown

instance websocketincomingJson :: DecodeJson WebsocketIncoming where
  decodeJson i = do
    o <- decodeJson i
    tag <- objectLookup "tag" o
    case tag of
      "InitializeEditor" -> do
        e <- objectLookup "editor" o
        pure (InitializeEditor e)
      "IncomingViewersUpdate" -> do
        vs <- objectLookup "viewers" o
        pure (IncomingViewersUpdate vs)
      _ -> pure Unknown

data Layout = OneColumn | TwoColumn | ThreeColumn
derive instance eqLayout :: Eq Layout

_editor = SProxy :: SProxy "editor"

newtype EditorSlot = EditorSlot SessionId
derive newtype instance eqEditorSlot :: Eq EditorSlot
derive newtype instance ordEditorSlot :: Ord EditorSlot

newtype EUpdate = EUpdate
  { title :: String
  , input :: String
  , selection :: Selection
  }

instance eupdate :: EncodeJson EUpdate where
  encodeJson (EUpdate editor) =
    ("title" := editor . title) ~>
    ("input" := editor . input) ~>
    ("selection" := editor . selection) ~>
    Json.jsonEmptyObject

data LayoutChoice = LayoutChoice
  { cls :: String
  , layout :: Layout
  }

--------------------------------------------------------------------------------
-- Component

component :: H.Component HH.HTML Query Input Void Aff
component =
  H.lifecycleParentComponent
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (Initialize unit)
    , finalizer: Nothing
    }
  where
    initialState :: Input -> State
    initialState (Input i) =
      State
        { sessionId: i . sessionId
        , room: i . room
        , viewers: []
        , mwebsocket: Nothing
        , layout: OneColumn
        , editor: Nothing
        }
    render (State state) =
      HH.div_
        ([ case state . editor of
             Nothing ->
               HH.div
                 [HP.class_ (ClassName "lds-ripple")]
                 [HH.div_ [], HH.div_ []]
             Just _ ->
               HH.div
                 [HP.class_ (ClassName "layout-choice")]
                 (map
                    (\(LayoutChoice choice) ->
                       HH.div
                         [ HP.class_
                             (ClassName
                                ("fas " <> choice . cls <>
                                 if choice . layout == state . layout
                                   then " selected"
                                   else ""))
                         , E.onClick (E.input_ (SetLayout choice . layout))
                         ]
                         [])
                    [ LayoutChoice {cls: "fa-stop", layout: OneColumn}
                    , LayoutChoice {cls: "fa-th-large", layout: TwoColumn}
                    , LayoutChoice {cls: "fa-th", layout: ThreeColumn}
                    ] <>
                  [ HH.div
                      [HP.class_ (ClassName "token-section")]
                      [ HH.text "Your token: "
                      , HH.span
                          [HP.class_ (ClassName "token")]
                          [ HH.text
                              (let SessionId s = state . sessionId
                                in s)
                          ]
                      ]
                  ])
         , HH.div
             [ HP.class_
                 (ClassName
                    ("grid " <>
                     case state . layout of
                       OneColumn -> "one-column"
                       TwoColumn -> "two-column"
                       ThreeColumn -> "three-column"))
             ]
             ((case state . editor of
                 Nothing -> []
                 Just e@(Editor editor) ->
                   [ HH.slot'
                       cp1
                       (EditorSlot (editor . session))
                       Editor.component
                       e
                       (\e' -> Just (OutgoingEditorUpdate e' unit))
                   ]) <>
              map
                (\e@(Editor editor) ->
                   HH.slot'
                     cp2
                     (EditorSlot (editor . session))
                     Viewer.component
                     e
                     (const Nothing))
                (state . viewers))
         ])
    eval ::
         Query ~> H.ParentDSL State Query (Coproduct2 Editor.Query Viewer.Query) (Either2 EditorSlot EditorSlot) Void Aff
    eval (SetLayout layout a) = do
      _ <- H.modify (\(State state) -> State (state {layout = layout}))
      pure a
    eval (Initialize a) = do
      State {sessionId} <- H.get
      websocket <- Websocket.connect
      Websocket.send websocket sessionId
      Websocket.subscribe websocket (either WebsocketError WebsocketIncoming)
      _ <-
        H.modify (\(State state) -> State (state {mwebsocket = Just websocket}))
      pure a
    eval (WebsocketError e a) = do
      H.liftEffect (log ("WebsocketError " <> e))
      pure a
    eval (WebsocketIncoming cmd a) =
      case cmd of
        (IncomingViewersUpdate viewers) -> do
          _ <- H.modify (\(State state) -> State (state {viewers = viewers}))
          pure a
        InitializeEditor editor -> do
          _ <- H.modify (\(State state) -> State (state {editor = Just editor}))
          pure a
        Unknown -> do
          H.liftEffect (log "WebsocketIncoming: Unknown")
          pure a
    eval (OutgoingEditorUpdate (Editor {title, input, selection}) a) = do
      State {mwebsocket} <- H.get
      case mwebsocket of
        Nothing -> pure a
        Just websocket -> do
          Websocket.send websocket (EUpdate {title, input, selection})
          pure a
