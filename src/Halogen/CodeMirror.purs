-- | Halogen API for CodeMirror.

module Halogen.CodeMirror where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (SubscribeStatus(..), eventSource)
import Prelude (type (~>), Unit, bind, const, discard, pure, unit, void, (/=))
import Web.HTML.HTMLElement (HTMLElement)

--------------------------------------------------------------------------------
-- Types

data Input = Input Config

type Config =
  { readOnly :: Boolean
  , theme    :: String
  , mode     :: String
  , value    :: String
  }

data State = State
  { codeMirror :: Maybe CodeMirror
  , config :: Config
  }

data Query a
  = Initializer a
  | InternalChange String a
  | Receive Config a

data Output =
  Change String

foreign import data CodeMirror :: Type

--------------------------------------------------------------------------------
-- Constants

refLabel :: H.RefLabel
refLabel = H.RefLabel "codemirror"

--------------------------------------------------------------------------------
-- Component

component :: H.Component HH.HTML Query Input Output Aff
component =
  H.lifecycleComponent
    { initialState: initialState
    , render
    , eval
    , receiver: receiver
    , finalizer: Nothing
    , initializer: Just (Initializer unit)
    }
  where
    initialState :: Input -> State
    initialState (Input config) = State {codeMirror: Nothing, config}
    receiver (Input c) = Just (Receive c unit)

render :: State -> H.ComponentHTML Query
render = const (HH.div [HP.ref refLabel] [])

eval :: Query ~> H.ComponentDSL State Query Output Aff
eval (Initializer a) = do
  State {config} <- H.get
  melement <- H.getHTMLElementRef refLabel
  case melement of
    Nothing -> pure a
    Just element -> do
      cm <- H.liftEffect (codeMirror element config)
      void
        (H.subscribe
           (eventSource
              (\callback ->
                 on
                   cm
                   "change"
                   (do value <- getValue cm
                       callback value))
              (\text ->
                 Just
                   (H.request (\next -> InternalChange text (next Listening))))))
      H.put (State {codeMirror: Just cm, config: config})
      pure a
eval (InternalChange text a) = do
  H.raise (Change text)
  pure a
eval (Receive config' a) = do
  State {codeMirror: mcm, config} <- H.get
  case mcm of
    Just cm ->
      if config' . value /= config . value
        then H.liftEffect (setValue cm (config' . value))
        else pure unit
    Nothing -> pure unit
  H.put (State {codeMirror: mcm, config: config'})
  pure a

--------------------------------------------------------------------------------
-- Foreign

foreign import codeMirror
  :: HTMLElement
  -> Config
  -> Effect CodeMirror

foreign import on
  :: CodeMirror
  -> String
  -> Effect Unit
  -> Effect Unit

foreign import getValue
  :: CodeMirror
  -> Effect String

foreign import setValue
  :: CodeMirror
  -> String
  -> Effect Unit
