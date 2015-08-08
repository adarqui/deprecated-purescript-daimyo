module Daimyo.Random.Xorshift.Xorshift64 (
  Xorshift64 (..),
  State64 (..),
  xorshift64,
  xorshifts64,
  seed64
) where

import Prelude
import Data.Int
import Data.Int.Bits
import Data.List
import Data.Tuple
import Control.Monad.State
import Control.Monad.State.Class

data Xorshift64 = Xorshift64 Int

type State64 = State Xorshift64 Int

-- | xorshift
--
-- >>> runState xorshift64 (Xorshift64 1)
-- (5180492295206395165,Xorshift64 33554433)
--
xorshift64 :: State64
xorshift64 = do
  (Xorshift64 x) <- get
  x <- return $ x .^. x `shr` 12
  x <- return $ x .^. x `shl` 25
  x <- return $ x .^. x `shr` 27
  put $ Xorshift64 x
  return $ x * 2685821657736338717

-- | xorshifts64
--
-- >>> xorshifts64 (Xorshift64 1) 10
-- ...
--
xorshifts64 :: Xorshift64 -> Int -> List Int
xorshifts64 seed n = go seed n
  where
  go _ 0 = Nil
  go g n = let st = runState xorshift64 g in (fst st) : go (snd st) (n-1)

-- | seed64
--
seed64 :: Xorshift64
seed64 = Xorshift64 9124824867239485709
