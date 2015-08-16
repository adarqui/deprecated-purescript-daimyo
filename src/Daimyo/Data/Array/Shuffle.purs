module Daimyo.Data.Array.Shuffle (
  shuffleEff,
  shuffleLCG_BSD,
  shuffleLCG_MS,
  pick1_BSD,
  pick1_MS,
  pick1
) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Data.Array
import Data.Maybe
import Data.Tuple

import Daimyo.Control.Monad.Array
import Daimyo.Random.LCG
import Daimyo.Random.LCG.BSD
import Daimyo.Random.LCG.MS

import Daimyo.Data.ArrayList
import qualified Daimyo.Data.List.Shuffle as S

-- | shuffleEff
--
-- shuffle a list.
-- a pure version of this would be great but i need this asap ;f
--
-- >>> shuffleEff (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons (2) (Cons (1) (Cons (3) (Nil)))
--
shuffleEff :: forall eff a. (Ord a) => Array a -> Eff (random :: RANDOM | eff) (Array a)
shuffleEff xs = listToArray <$> S.shuffleEff (arrayToList xs)

-- | shuffleLCG_BSD
--
shuffleLCG_BSD :: forall a. (Ord a) => Int -> Array a -> Array a
shuffleLCG_BSD seed xs = listToArray $ S.shuffleLCG_BSD seed (arrayToList xs)

-- | shuffleLCG_MS
--
shuffleLCG_MS :: forall a. (Ord a) => Int -> Array a -> Array a
shuffleLCG_MS seed xs = listToArray $ S.shuffleLCG_MS seed (arrayToList xs)

-- | pick1_BSD
--
pick1_BSD :: forall a. Int -> Array a -> Maybe a
pick1_BSD seed xs = S.pick1_BSD seed (arrayToList xs)

-- | pick1_MS
--
pick1_MS :: forall a. Int -> Array a -> Maybe a
pick1_MS seed xs = S.pick1_MS seed (arrayToList xs)

-- | pick1
--
pick1 :: forall a. Int -> Array a -> Maybe a
pick1 seed xs = S.pick1 seed (arrayToList xs)
