module Daimyo.Random.LCG.MS (
  lcgMS,
  lcgsMS,
  runLCGMS
) where

import Prelude
import Data.List
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans

import Daimyo.Random
import Daimyo.Random.LCG

-- | lcgMS
--
lcgMS :: State Int Int
lcgMS = lcg (LCG 214013 2531011 twoPow31)

-- | lcgsMS
--
-- >>> take 5 $ lcgsMS 5
-- [3601076,1880463015,803157710,1602335321,1812736952]
--
lcgsMS :: Int -> Int -> List Int
lcgsMS seed n = evalState (replicateM n lcgMS) seed

-- | runLCGMS
--
runLCGMS :: Int -> Int
runLCGMS = evalState lcgMS
