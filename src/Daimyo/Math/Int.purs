module Daimyo.Math.Int (
  abs
) where

import Prelude
import Data.Int

abs :: Int -> Int
abs x
  | x < 0     = x * (-1)
  | otherwise = x
