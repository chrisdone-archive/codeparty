-- |

module CodeParty.Components.Viewer where

import CodeParty.Types
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.CodeMirror as CodeMirror
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP
import Prelude (class Eq, class Ord, type (~>), Unit, const, discard, pure, unit, (==))

--------------------------------------------------------------------------------
-- Types

data State = State { viewer :: Editor }

data Query a =
  ReceiveViewer Editor a

_codeMirror = SProxy :: SProxy "codeMirror"

newtype CodeMirrorSlot = CodeMirrorSlot SessionId
derive newtype instance eqCodeMirrorSlot :: Eq CodeMirrorSlot
derive newtype instance ordCodeMirrorSlot :: Ord CodeMirrorSlot

--------------------------------------------------------------------------------
-- Component

component :: H.Component HH.HTML Query Editor Unit Aff
component = H.parentComponent {initialState, render, eval, receiver}
  where
    receiver i = Just (ReceiveViewer i unit)
    initialState :: Editor -> State
    initialState (Editor e) = State {viewer: Editor e}
    render (State {viewer: Editor viewer}) =
      HH.div
        [ HP.class_
            (ClassName
               ("editor " <>
                ("theirs " <>
                 if viewer . title == ""
                   then "waiting"
                   else "")))
        ]
        [ HH.div
            [HP.class_ (ClassName "title")]
            [ HH.text
                (if viewer . title == ""
                   then "[Waiting for other participant]"
                   else viewer . title)
            ]
        , HH.div
            [HP.class_ (ClassName "input readonly")]
            [ HH.slot
                (CodeMirrorSlot (viewer . session))
                CodeMirror.component
                (CodeMirror.Input
                   { value: viewer . input
                   , readOnly: true
                   , theme: "tomorrow-night-eighties"
                   , mode: "haskell"
                   , selection:
                       let Selection range = viewer . selection
                        in range
                   , styleActiveLine: true
                   , lineNumbers: true
                   , lineWrapping: true
                   })
                (const Nothing)
            ]
        , HH.div [HP.class_ (ClassName "output")] [HH.text (viewer . output)]
        ]
    eval :: Query ~> H.ParentDSL State Query CodeMirror.Query CodeMirrorSlot Unit Aff
    eval (ReceiveViewer (Editor viewer') a) = do
      H.put (State {viewer: Editor viewer'})
      pure a
