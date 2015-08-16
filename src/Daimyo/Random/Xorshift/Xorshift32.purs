module Daimyo.Random.Xorshift.Xorshift32 (
  Xorshift32 (..),
  State32 (..),
  xorshift32,
  xorshifts32,
  seed32,
  fromXorshift32,
  runXorshift32
) where

import Prelude
import Data.Int
import Data.Int.Bits
import Data.List
import Data.Tuple
import Control.Monad.State
import Control.Monad.State.Class

data Xorshift32 = Xorshift32 Int

type State32 = State Xorshift32 Int

-- | xorshift
--
-- >>> runState xorshift32 (Xorshift32 1)
--
xorshift32 :: State32
xorshift32 = do
  (Xorshift32 x) <- get
  x <- return $ x .^. x `shl` 13
  x <- return $ x .^. x `shr` 17
  x <- return $ x .^. x `shl` 5
  put $ Xorshift32 x
  return x

-- | xorshifts32
--
-- >>> xorshifts32 (Xorshift32 1) 10
-- ...
--
xorshifts32 :: Xorshift32 -> Int -> List Int
xorshifts32 seed n = go seed n
  where
  go _ 0 = Nil
  go g n = let st = runState xorshift32 g in (fst st) : go (snd st) (n-1)

-- | seed32
--
seed32 :: Xorshift32
seed32 = Xorshift32 7

-- | fromXorshift32
--
fromXorshift32 :: Xorshift32 -> Int
fromXorshift32 (Xorshift32 i) = i

-- | runXorshift32
--
runXorshift32 :: Int -> Int
runXorshift32 seed = evalState xorshift32 (Xorshift32 seed)
