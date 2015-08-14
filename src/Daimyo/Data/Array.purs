module Daimyo.Data.Array (
  intersperse
) where

import Prelude
import Data.Array
import Data.Maybe

-- | intersperse
--
-- >>> intersperse ',' "abcde" == "a,b,c,d,e"
--
intersperse :: forall a. a -> Array a -> Array a
intersperse sep ys  = case uncons ys of
                           Nothing                   -> []
                           Just { head: h, tail: t } -> h : prependToAll sep t

-- | prependToAll
--
prependToAll :: forall a. a -> Array a -> Array a
prependToAll sep ys = case uncons ys of
                           Nothing                   -> []
                           Just { head: h, tail: t } -> sep : h : prependToAll sep t
