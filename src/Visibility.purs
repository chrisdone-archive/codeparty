-- |

module Visibility where

import Effect (Effect)

foreign import isDocumentHidden :: Effect Boolean
