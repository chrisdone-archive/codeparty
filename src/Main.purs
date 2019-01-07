module Main where

import CodeParty.Components.Party as Party
import CodeParty.Session
import CodeParty.Types
import Data.Maybe
import Effect (Effect)
import Effect.Console
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude

main :: Effect Unit
main = do
  sid@(SessionId s) <- acquireSessionId
  room@(Room roomString) <- lookupRoom
  log ("Room: " <> roomString)
  log ("Session: " <> s)
  HA.runHalogenAff
    (do body <- HA.awaitBody
        runUI Party.component (Party.Input {sessionId: sid, room: room}) body)
