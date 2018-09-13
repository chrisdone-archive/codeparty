-- |

module CodeParty.Components.Editor where

import CodeParty.Types (Editor(..), SessionId)
import Data.Maybe (Maybe(..))
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
  = SetEditor Editor
              a
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
        [HP.class_ (ClassName "editor")]
        [ HH.div
            [HP.class_ (ClassName "title")]
            [ HH.input
                [ HP.value (editor . title)
                , HP.type_ HP.InputText
                , HP.placeholder (if editor . session == sessionId
                                     then "Type your name here"
                                     else "Another participant")
                , E.onValueInput
                    (\i -> Just (SetEditor (Editor (editor {title = i})) unit))
                , HP.disabled (not (editor . session == sessionId))
                ]
            ]
        , HH.div
            [HP.class_ (ClassName "input")]
            [ HH.slot
                (CodeMirrorSlot (editor . session))
                CodeMirror.component
                (CodeMirror.Input
                   { value: editor . input
                   , readOnly: not (editor . session == sessionId)
                   , theme: "zenburn"
                   , mode: "haskell"
                   })
                (\i ->
                   Just
                     (case i of
                        CodeMirror.Change text ->
                          SetEditor (Editor (editor {input = text})) unit))
            ]
        , HH.div [HP.class_ (ClassName "output")] [HH.text (editor . output)]
        ]
    eval ::
         Query ~> H.ParentDSL State Query CodeMirror.Query CodeMirrorSlot Editor Aff
    eval (SetEditor e a) = do
      H.raise e
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
