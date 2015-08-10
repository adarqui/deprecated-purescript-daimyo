module Daimyo.Control.Monad (
  (>>)
) where

import Prelude
import Data.Maybe
import Data.Tuple
import Control.Monad

(>>) :: forall m a b. (Bind m) => m a -> m b -> m b
(>>) x y = x >>= const y
