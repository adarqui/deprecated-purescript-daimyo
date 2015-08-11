module Daimyo.Data.String (
  findAllOccurrences
) where

import Prelude
import Data.Maybe
import Data.String
import Data.Tuple
import qualified Data.List as L

-- | findAllOccurences
--
-- find all occurrences of s1 in s2
--
-- true indicates a match
-- false indicates no match
--
-- >>> findAllOccurrences " hi " "yo hi"
-- Cons (Tuple (false) ("yo hi")) (Nil)
--
-- >>> findAllOccurrences "hi" "yo hi"
-- Cons (Tuple (false) ("yo ")) (Cons (Tuple (true) ("hi")) (Nil))
--
-- >>> findAllOccurrences " a " "id :: a -> a "
-- Cons (Tuple (false) ("id ::")) (Cons (Tuple (true) (" a ")) (Cons (Tuple (false) ("->")) (Cons ( Tuple (true) (" a ")) (Nil))))
--
findAllOccurrences :: String -> String -> L.List (Tuple Boolean String)
findAllOccurrences s1 s2 = go L.Nil s2
  where
  len = length s1
  go acc a2 =
    case indexOf s1 a2 of
         Nothing -> if (length a2 == 0)
                       then L.reverse acc
                       else L.reverse (Tuple false a2 L.: acc)
         Just i  -> if i == 0 then go (Tuple true s1 L.: acc) (drop len a2)
                              else go ((Tuple true s1) L.: Tuple false (take i a2) L.: acc) (drop (i+len) a2)
