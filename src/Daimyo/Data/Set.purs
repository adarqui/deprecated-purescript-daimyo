module Daimyo.Data.Set (
  filter
) where

import Prelude
import qualified Data.List as L
import qualified Data.Set as S

filter :: forall a. (a -> Boolean) -> S.Set a -> L.List a
filter f = L.filter f <<< S.toList
