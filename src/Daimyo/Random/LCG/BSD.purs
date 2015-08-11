module Daimyo.Random.LCG.BSD (
  lcgBSD,
  lcgsBSD
) where

import Prelude
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans

import Daimyo.Random
import Daimyo.Random.LCG
import Daimyo.Control.Monad.List

-- | lcgBSD
--
lcgBSD :: State Int Int
lcgBSD = lcg (LCG 1103515245 12345 twoPow31)

-- | lcgsBSD
--
-- >>> take 5 $ lcgsBSD 5
-- [1222621274,554244747,695785320,2089129857,668008486]
--
lcgsBSD :: Int -> Int -> List Int
lcgsBSD seed n = evalState (replicateM n lcgBSD) seed
