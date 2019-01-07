-- |

module CodeParty.Components.Editor where

import CodeParty.Types
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.CodeMirror as CodeMirror
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import Prelude (class Eq, class Ord, type (~>), bind, discard, pure, unit)

--------------------------------------------------------------------------------
-- Types

data State = State { editor :: Editor }

data Query a
  = SetEditorContent { selection :: Selection, input:: String }
             a
  | SetEditorTitle String a

_codeMirror = SProxy :: SProxy "codeMirror"

newtype CodeMirrorSlot = CodeMirrorSlot SessionId
derive newtype instance eqCodeMirrorSlot :: Eq CodeMirrorSlot
derive newtype instance ordCodeMirrorSlot :: Ord CodeMirrorSlot

--------------------------------------------------------------------------------
-- Component

component :: H.Component HH.HTML Query Editor Editor Aff
component =
  H.parentComponent {initialState, render, eval, receiver}
  where
    receiver _ = Nothing
    initialState :: Editor -> State
    initialState (Editor e) =
      State
        { editor: Editor e
        }
    render (State {editor: Editor editor}) =
      HH.div
        [ HP.class_
            (ClassName
               ("editor mine"))
        ]
        [ HH.div
            [HP.class_ (ClassName "title")]
            [ HH.input
                [ HP.value (editor . title)
                , HP.type_ HP.InputText
                , HP.placeholder"Type your name here"
                , E.onValueInput (\i -> Just (SetEditorTitle i unit))
                ]
            ]
        , HH.div
            [ HP.class_
                (ClassName
                   ("input editable"))
            ]
            [ HH.slot
                (CodeMirrorSlot (editor . session))
                CodeMirror.component
                (CodeMirror.Input
                   { value: editor . input
                   , readOnly: false
                   , theme: "tomorrow-night-eighties"
                   , mode: "haskell"
                   , selection:
                       let Selection range = editor . selection
                        in range
                   , styleActiveLine: true
                   , lineNumbers: true
                   , lineWrapping: true
                   })
                (\i ->
                   Just
                     (case i of
                        CodeMirror.Change {value, selection} ->
                          SetEditorContent
                            {input: value, selection: Selection selection}
                            unit))
            ]
        , HH.div [HP.class_ (ClassName "output")] [HH.text (editor . output)]
        ]
    eval ::
         Query ~> H.ParentDSL State Query CodeMirror.Query CodeMirrorSlot Editor Aff
    eval (SetEditorTitle title a) = do
      State {editor} <-
        H.modify
          (\(State s) ->
             State
               (s
                  { editor =
                      let Editor e = s . editor
                       in Editor (e {title = title})
                  }))
      H.raise editor
      pure a
    eval (SetEditorContent u a) = do
      State {editor} <-
        H.modify
          (\(State s) ->
             State
               (s
                  { editor =
                      let Editor e = s . editor
                       in Editor
                            (e {selection = u . selection, input = u . input})
                  }))
      H.raise editor
      pure a
