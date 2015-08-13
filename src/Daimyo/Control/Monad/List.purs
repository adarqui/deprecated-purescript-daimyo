module Daimyo.Control.Monad.List (
  mapM,
  mapM_,
  replicateM_
) where


import Prelude
import Data.List
import Data.Maybe
import Data.Tuple

mapM :: forall eff m a b. (Monad m, Applicative m) => (a -> m b) -> List a -> m (List b)
mapM f xs = do
  let
    h = head xs
    t = tail xs
  case Tuple h t of
       Tuple Nothing _ -> return Nil
       Tuple (Just h') (Just t') -> do
         y  <- f h'
         ys <- mapM f t'
         return (y : ys)

mapM_ :: forall eff m a b. (Monad m, Applicative m) => (a -> m b) -> List a -> m Unit
mapM_ f xs = do
  _ <- mapM f xs
  return unit

replicateM' :: forall eff m a. (Monad m, Applicative m) => Int -> m a -> m (List a)
replicateM' 0 _ = return Nil
replicateM' n f = do
  x  <- f
  xs <- replicateM' (n-1) f
  return (Cons x xs)

replicateM_ :: forall eff m a. (Monad m, Applicative m) => Int -> m a -> m Unit
replicateM_ n f = do
  _ <- replicateM n f
  return unit
