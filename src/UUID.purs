-- | Quick and easy UUID generation.

module UUID where

import Effect (Effect)

foreign import makeUUID :: Effect String
