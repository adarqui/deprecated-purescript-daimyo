module Daimyo.Data.List.Shuffle (
  shuffleEff
) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.List
import Data.Tuple

import Daimyo.Control.Monad.List

-- | shuffleEff
--
-- shuffle a list.
-- a pure version of this would be great but i need this asap ;f
--
-- >>> shuffleEff (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons (2) (Cons (1) (Cons (3) (Nil)))
--
shuffleEff :: forall eff a. (Ord a) => List a -> Eff (random :: RANDOM | eff) (List a)
shuffleEff xs = (map snd <<< sort) <$> mapM (\x -> randomInt 0 99999999 >>= \i -> return $ Tuple i x) xs
