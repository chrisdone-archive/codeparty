-- |

module CodeParty.Types where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Foreign.Object as FO
import Halogen.CodeMirror as CodeMirror
import Prelude

data Editor = Editor
  { session :: SessionId
  , title   :: String
  , input   :: String
  , output  :: String
  , selection :: Selection
  }

instance editorJson2 :: EncodeJson Editor where
  encodeJson (Editor { session: SessionId session
                     , title
                     , input
                     , output
                     , selection
                     }) =
    ("session" := (encodeJson session)) ~> ("title" := (encodeJson title)) ~>
    ("input" := (encodeJson input)) ~>
    ("output" := (encodeJson output)) ~>
    ("selection" := (encodeJson selection)) ~>
    Json.jsonEmptyObject

instance editorJson :: DecodeJson Editor where
  decodeJson i = do
    o <- decodeJson i
    session <- objectLookup "session" o
    title <- objectLookup "title" o
    input <- objectLookup "input" o
    output <- objectLookup "output" o
    selection <- objectLookup "selection" o
    pure (Editor {session, title, input, output, selection})

newtype Selection = Selection CodeMirror.Range

instance selectionJson2 :: EncodeJson Selection where
  encodeJson (Selection {head, anchor: tail}) =
    ("head" := headj) ~> ("tail" := tailj) ~> Json.jsonEmptyObject
    where
      headj =
        (("line" := encodeJson (head . line)) ~>
         ("ch" := encodeJson (head . ch)) ~>
         Json.jsonEmptyObject)
      tailj =
        (("line" := encodeJson (tail . line)) ~>
         ("ch" := encodeJson (tail . ch)) ~>
         Json.jsonEmptyObject)

instance selectionJson :: DecodeJson Selection where
  decodeJson i = do
    o <- decodeJson i
    head <-
      do head <- objectLookup "head" o
         line <- objectLookup "line" head
         ch <- objectLookup "ch" head
         pure {line, ch}
    tail <-
      do tail <- objectLookup "tail" o
         line <- objectLookup "line" tail
         ch <- objectLookup "ch" tail
         pure {line, ch}
    pure (Selection {head, anchor: tail})

newtype SessionId = SessionId String
derive newtype instance eqSessionId :: Eq SessionId
derive newtype instance ordSessionId :: Ord SessionId

instance sessionJson :: DecodeJson SessionId where
  decodeJson i = do
    o <- decodeJson i
    pure (SessionId o)

instance sessionIdJson2 :: EncodeJson SessionId where
  encodeJson (SessionId i) =
    encodeJson i

newtype Room = Room String

instance roomJson :: DecodeJson Room where
  decodeJson i = do
    o <- decodeJson i
    pure (Room o)

objectLookup :: forall a. DecodeJson a => String -> FO.Object Json -> Either String a
objectLookup key fo =
  maybe (Left ("Expected key:" <> key)) decodeJson (FO.lookup key fo)
