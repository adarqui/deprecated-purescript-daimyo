module Daimyo.Data.ArrayList where

import Prelude
import Data.Foldable
import qualified Data.Array as A
import qualified Data.List as L

-- | listToArray
--
-- convert a list to an array: not sure if this is efficient yet
--
listToArray :: forall a. L.List a -> Array a
listToArray = go []
  where
  go acc L.Nil         = A.reverse acc
  go acc (L.Cons x xs) = go (x A.: acc) xs

-- | arrayToList
--
-- convert an array to a list
--
arrayToList :: forall a. Array a -> L.List a
arrayToList xs = L.reverse $ foldl (\acc x -> (x L.: acc)) L.Nil xs
