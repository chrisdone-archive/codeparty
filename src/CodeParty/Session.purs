-- | Session ID generation.

module CodeParty.Session where

import CodeParty.Types (Room(..), SessionId(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.String as String
import Effect (Effect)
import Location
import Prelude (bind, discard, pure)
import SessionStorage (getItem, putItem)
import UUID (makeUUID)

lookupRoom :: Effect Room
lookupRoom = do
  path <- getPathname
  pure (Room (String.drop 1 path))

acquireSessionId :: Effect SessionId
acquireSessionId = do
  item <- getItem key
  case toMaybe item of
    Nothing -> do
      uuid <- makeUUID
      putItem key uuid
      pure (SessionId uuid)
    Just uuid -> pure (SessionId uuid)
  where
    key = "sessionid"
