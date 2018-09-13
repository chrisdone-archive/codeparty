-- | Websocket API for Halogen.

module Halogen.Websocket where

import Data.Argonaut.Core (stringify) as Json
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser) as Json
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Canceler(..), makeAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Query.EventSource (SubscribeStatus(..), eventSource)
import Prelude (Unit, bind, discard, pure, unit, (>>=))

--------------------------------------------------------------------------------
-- Types

foreign import data Websocket :: Type

--------------------------------------------------------------------------------
-- Halogen API

connect :: forall m. MonadAff m => m Websocket
connect =
  H.liftAff
    (makeAff
       (\callback -> do
          ws <- connectRaw
          onopenRaw ws (callback (Right ws))
          pure (Canceler (\e -> pure unit))))

send :: forall m msg. MonadEffect m => EncodeJson msg => Websocket -> msg -> m Unit
send conn str = H.liftEffect (sendRaw conn (Json.stringify (encodeJson str)))

subscribe ::
     forall m t4 t5 t6 f t8 msg. MonadAff m
  => DecodeJson msg
  => Websocket
  -> (Either String msg -> SubscribeStatus -> f SubscribeStatus)
  -> H.HalogenM t8 f t6 t5 t4 m Unit
subscribe socket query =
  H.subscribe
    (eventSource
       (onmessageRaw socket)
       (\text ->
          Just
            (H.request
               (\next -> query (Json.jsonParser text >>= decodeJson) Listening))))

--------------------------------------------------------------------------------
-- FFI

foreign import connectRaw :: Effect Websocket

foreign import sendRaw :: Websocket -> String -> Effect Unit

foreign import onmessageRaw :: Websocket -> (String -> Effect Unit) -> Effect Unit

foreign import onopenRaw :: Websocket -> Effect Unit -> Effect Unit
