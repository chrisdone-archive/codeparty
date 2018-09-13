-- | Quick and easy session storage access.

module SessionStorage where

import Data.Nullable (Nullable)
import Prelude (Unit)
import Effect (Effect)

foreign import putItem :: String -> String -> Effect Unit

foreign import getItem :: String -> Effect (Nullable String)
