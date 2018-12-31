-- |

module CodeParty.Components.Editor where

import CodeParty.Types
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.CodeMirror as CodeMirror
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import Prelude (class Eq, class Ord, type (~>), bind, discard, not, pure, unit, (==))

--------------------------------------------------------------------------------
-- Types

data State = State { editor :: Editor, access :: Access }

data Access = Everything | Output

data Query a
  = SetEditorContent { selection :: Selection, input:: String }
             a
  | SetEditorTitle String a
  | ReceiveEditor Editor
                  a

_codeMirror = SProxy :: SProxy "codeMirror"

newtype CodeMirrorSlot = CodeMirrorSlot SessionId
derive newtype instance eqCodeMirrorSlot :: Eq CodeMirrorSlot
derive newtype instance ordCodeMirrorSlot :: Ord CodeMirrorSlot

--------------------------------------------------------------------------------
-- Component

component :: SessionId -> H.Component HH.HTML Query Editor Editor Aff
component sessionId =
  H.parentComponent {initialState: initialState, render, eval, receiver}
  where
    receiver i = Just (ReceiveEditor i unit)
    initialState :: Editor -> State
    initialState (Editor e) =
      State
        { editor: Editor e
        , access:
            if e . session == sessionId
              then Output
              else Everything
        }
    render (State {editor: Editor editor}) =
      HH.div
        [ HP.class_
            (ClassName
               ("editor " <>
                if editor . session == sessionId
                  then "mine"
                  else "theirs"))
        ]
        [ HH.div
            [HP.class_ (ClassName "title")]
            [ if editor . session == sessionId
                 then HH.input
                        [ HP.value (editor . title)
                        , HP.type_ HP.InputText
                        , HP.placeholder
                            (if editor . session == sessionId
                               then "Type your name here"
                               else "")
                        , E.onValueInput (\i -> Just (SetEditorTitle i unit))
                        , HP.disabled (not (editor . session == sessionId))
                        ]
                 else HH.text (editor . title)
            ]
        , HH.div
            [ HP.class_
                (ClassName
                   ("input " <>
                    if editor . session == sessionId
                      then "editable"
                      else "readonly"))
            ]
            [ HH.slot
                (CodeMirrorSlot (editor . session))
                CodeMirror.component
                (CodeMirror.Input
                   { value: editor . input
                   , readOnly: not (editor . session == sessionId)
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
    eval (ReceiveEditor (Editor editor') a) = do
      State {editor: Editor editor, access} <- H.get
      case access of
        Output -> do
          H.put
            (State
               {editor: Editor (editor {output = editor' . output}), access})
        Everything -> do
          H.put (State {editor: Editor editor', access})
      pure a
