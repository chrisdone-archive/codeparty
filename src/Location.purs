-- | Quick and easy window.location access.

module Location where

import Effect (Effect)

foreign import getPathname :: Effect String
