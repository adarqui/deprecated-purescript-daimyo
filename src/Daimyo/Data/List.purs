module Daimyo.Data.List (
  intersperse,
  prependToAll,
  elem
) where

import Prelude
import Data.List
import Data.Maybe

-- | intersperse
--
-- >>> intersperse ',' "abcde" == "a,b,c,d,e"
--
intersperse :: forall a. a -> List a -> List a
intersperse sep ys  = case uncons ys of
                           Nothing                   -> Nil
                           Just { head: h, tail: t } -> h : prependToAll sep t

-- | prependToAll
--
prependToAll :: forall a. a -> List a -> List a
prependToAll sep ys = case uncons ys of
                           Nothing                   -> Nil
                           Just { head: h, tail: t } -> sep : h : prependToAll sep t



-- | elem
--
elem :: forall a. Eq a => a -> List a -> Maybe a
elem a l = go l
  where
    go Nil = Nothing
    go z =
      case uncons z of
        Nothing -> Nothing
        Just {head:x, tail:xs} ->
          if x == a
            then (Just a)
            else (go xs)
