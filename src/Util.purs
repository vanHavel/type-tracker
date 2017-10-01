module Util where

import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

unsafeFromMaybe :: forall a. Maybe a -> a
unsafeFromMaybe (Just a) = a
unsafeFromMaybe Nothing = unsafeCrashWith "unsafe from maybe failed"
