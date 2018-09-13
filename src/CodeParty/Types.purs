-- |

module CodeParty.Types where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Prelude

data Editor = Editor
  { session :: SessionId
  , title   :: String
  , input   :: String
  , output  :: String
  }

instance editorJson2 :: EncodeJson Editor where
  encodeJson (Editor {session: SessionId session, title, input, output}) =
    encodeJson
      (M.fromFoldable
         [ Tuple "session" (encodeJson session)
         , Tuple "title" (encodeJson title)
         , Tuple "input" (encodeJson input)
         , Tuple "output" (encodeJson output)
         ])

instance editorJson :: DecodeJson Editor where
  decodeJson i = do
    o <- decodeJson i
    session <- objectLookup "session" o
    title <- objectLookup "title" o
    input <- objectLookup "input" o
    output <- objectLookup "output" o
    pure (Editor {session, title, input, output})

objectLookup :: forall a. DecodeJson a => String -> FO.Object Json -> Either String a
objectLookup key fo =
  maybe (Left ("Expected key:" <> key)) decodeJson (FO.lookup key fo)

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
