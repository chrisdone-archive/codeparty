-- | This is where the editors are all brought together.

module CodeParty.Components.Party where

import CodeParty.Components.Editor as Editor
import CodeParty.Types
import Data.Argonaut.Core as Json
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array as Array
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
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
  , editors :: Array Editor
  , layout :: Layout
  }

data Query a
  = Initialize a
  | WebsocketError String a
  | IncomingEditorsUpdate (Array Editor) a
  | OutgoingEditorUpdate Editor a
  | SetLayout Layout a

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
        , editors: []
        , mwebsocket: Nothing
        , layout: OneColumn
        }
    render (State state) =
      HH.div_
        ([ if Array.null (state . editors)
             then HH.div
                    [HP.class_ (ClassName "lds-ripple")]
                    [HH.div_ [], HH.div_ []]
             else HH.div
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
                       , LayoutChoice
                           {cls: "fa-th-large", layout: TwoColumn}
                       , LayoutChoice {cls: "fa-th", layout: ThreeColumn}
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
             (map
                (\e@(Editor editor) ->
                   HH.slot
                     (EditorSlot (editor . session))
                     (Editor.component (state . sessionId))
                     e
                     (\e' -> Just (OutgoingEditorUpdate e' unit)))
                (Array.sortBy
                   (comparing (\(Editor e) -> e . session /= state . sessionId))
                   (state . editors)))
         ])
    eval :: Query ~> H.ParentDSL State Query Editor.Query EditorSlot Void Aff
    eval (SetLayout layout a) = do
      _ <- H.modify (\(State state) -> State (state {layout = layout}))
      pure a
    eval (Initialize a) = do
      State {sessionId} <- H.get
      websocket <- Websocket.connect
      Websocket.send websocket sessionId
      Websocket.subscribe
        websocket
        (either WebsocketError IncomingEditorsUpdate)
      _ <-
        H.modify (\(State state) -> State (state {mwebsocket = Just websocket}))
      pure a
    eval (WebsocketError e a) = do
      H.liftEffect (log ("WebsocketError " <> e))
      pure a
    eval (IncomingEditorsUpdate editors a) = do
      _ <-
        H.modify
          (\(State state) ->
             State
               (state
                  { editors = editors
                  , layout =
                      case Array.length editors of
                        1 -> OneColumn
                        2 -> TwoColumn
                        3 -> ThreeColumn
                        _ -> ThreeColumn
                  }))
      pure a
    eval (OutgoingEditorUpdate (Editor {title, input, selection}) a) = do
      State {mwebsocket} <- H.get
      case mwebsocket of
        Nothing -> pure a
        Just websocket -> do
          Websocket.send websocket (EUpdate {title, input, selection})
          pure a
