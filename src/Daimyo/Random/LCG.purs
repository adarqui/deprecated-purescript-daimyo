module Daimyo.Random.LCG (
  LCG (..),
  lcg
) where

-- Linear Congruential Generator
-- https://en.wikipedia.org/wiki/Linear_congruential_generator

import Prelude
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans

data LCG
  = LCG Int Int Int
--      a   c   m

-- | lcg
--
lcg :: LCG -> State Int Int
lcg (LCG a c m) = do
  modify (\st -> (a * st + c) `mod` m)
  get
