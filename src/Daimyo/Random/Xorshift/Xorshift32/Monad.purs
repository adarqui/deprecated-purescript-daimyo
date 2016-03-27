module Daimyo.Random.Xorshift.Xorshift32.Monad where

import Prelude
import Data.Identity
import Data.Monoid
import Data.Tuple
import Daimyo.Random.Xorshift.Xorshift32
import Control.Alt
import Control.Alternative
import Control.Lazy
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Trans
import Control.MonadPlus
import Control.Plus
import Control.Monad.State.Trans
-- import Control.Monad.Error
-- import Control.Monad.Error.Trans
import Control.Monad.Maybe.Trans
import Control.Monad.Reader.Trans
import Control.Monad.RWS.Trans
import Control.Monad.Writer.Trans

newtype Xorshift32T m a = Xorshift32T (Int -> m (Tuple a Int))

runXorshift32T :: forall m a. Xorshift32T m a -> Int -> m (Tuple a Int)
runXorshift32T (Xorshift32T s) = s

evalXorshift32T :: forall m a. (Apply m) => Xorshift32T m a -> Int -> m a
evalXorshift32T m n = fst <$> runXorshift32T m n

execXorshift32T :: forall m a. (Apply m) => Xorshift32T m a -> Int -> m Int
execXorshift32T m n = snd <$> runXorshift32T m n

-- | Functor
--
-- map    :: forall a b f. (Prelude.Functor f) => (a -> b) -> f a -> f b
--
instance functorXorshift32T :: (Monad m) => Functor (Xorshift32T m) where
  map = liftM1

-- | Apply
--
-- apply :: forall a b f. (Prelude.Apply f) => f (a -> b) -> f a -> f b
--
instance applyXorshift32T :: (Monad m) => Apply (Xorshift32T m) where
  apply = ap

-- | Applicative
--
-- pure :: forall a f. (Prelude.Applicative f) => a -> f a
--
instance applicativeXorshift32T :: (Monad m) => Applicative (Xorshift32T m) where
  pure a = Xorshift32T $ \n -> return $ Tuple a n

-- | Alt
--
-- alt :: forall a f. (Control.Alt.Alt f) => f a -> f a -> f a
--
instance altXorshift32T :: (Monad m, Alt m) => Alt (Xorshift32T m) where
  alt x y = Xorshift32T $ \n -> runXorshift32T x n <|> runXorshift32T y n

-- | Plus
--
-- empty :: forall a f. (Control.Plus.Plus f) => f a
--
instance plusXorshift32T :: (Monad m, Plus m) => Plus (Xorshift32T m) where
  empty = Xorshift32T $ \_ -> empty

-- | Alternative
--
instance alternativeXorshift32T :: (Monad m, Alternative m) => Alternative (Xorshift32T m)

-- | Bind
--
-- bind :: forall a b m. (Prelude.Bind m) => m a -> (a -> m b) -> m b
--
instance bindXorshift32T :: (Monad m) => Bind (Xorshift32T m) where
  bind (Xorshift32T x) f = Xorshift32T \n -> do
    Tuple v n' <- x (runXorshift32 n)
    runXorshift32T (f v) n'

-- | Monad
--
instance monadXorshift32T :: (Monad m) => Monad (Xorshift32T m)

-- | MonadTrans
--
-- lift :: forall m a t. (Control.Monad.Trans.MonadTrans t, Prelude.Monad m) => m a -> t m a
--
instance monadTransStateT :: MonadTrans Xorshift32T where
  lift m = Xorshift32T \n -> do
    x <- m
    return $ Tuple x n

-- | Lazy
--
-- defer :: forall l. (Control.Lazy.Lazy l) => (Prelude.Unit -> l) -> l
--
instance lazyXorshift32T :: Lazy (Xorshift32T m a) where
  defer f = Xorshift32T $ \n -> runXorshift32T (f unit) n

-- | MonadEff
--
-- liftEff :: forall a m eff. (Control.Monad.Eff.Class.MonadEff eff m) => Control.Monad.Eff.Eff eff a -> m a
--
instance monadEffXorshift32T :: (Monad m, MonadEff eff m) => MonadEff eff (Xorshift32T m) where
  liftEff = lift <<< liftEff

class MonadXorshift32 m where
  seed :: forall a. (Int -> (Tuple a Int)) -> m a

getSeed :: forall m. (Monad m, MonadXorshift32 m) => m Int
getSeed = seed \n -> Tuple n n

instance monadXorshift32Xorshift32T :: (Monad m) => MonadXorshift32 (Xorshift32T m) where
  seed f = Xorshift32T $ return <<< f

instance monadXorshift32Xorshift32T1 :: (Monad m, MonadXorshift32 m) => MonadXorshift32 (Xorshift32T m) where
  seed f = lift (seed f)

instance monadXorshift32StateT :: (Monad m, MonadXorshift32 m) => MonadXorshift32 (StateT s m) where
  seed f = lift (seed f)

-- instance monadXorshift32ErrorT :: (Monad m, MonadXorshift32 m) => MonadXorshift32 (ErrorT e m) where
--  seed f = lift (seed f)

instance monadXorshift32MaybeT :: (Monad m, MonadXorshift32 m) => MonadXorshift32 (MaybeT m) where
  seed f = lift (seed f)

instance monadXorshift32ReaderT :: (Monad m, MonadXorshift32 m) => MonadXorshift32 (ReaderT r m) where
  seed f = lift (seed f)

instance monadXorshift32WriterT :: (Monad m, Monoid w, MonadXorshift32 m) => MonadXorshift32 (WriterT w m) where
  seed f = lift (seed f)

-- | test
-- >>> runXorshift32T t 5
-- 1351845
-- 1057120550
-- Tuple ("hi") (401968661)
--
t :: forall eff. Xorshift32T (Eff (console :: CONSOLE | eff)) String
t = do
  s1 <- getSeed
  lift $ log (show s1)
  s2 <- getSeed
  lift $ log (show s2)
  return "hi"


-- would name this Xorshift32 but.. already taken.. 'I' for identity
type Xorshift32I = Xorshift32T Identity

runXorshift32I :: forall a. Xorshift32I a -> Int -> Tuple a Int
runXorshift32I m n = runIdentity $ runXorshift32T m n

evalXorshift32I :: forall a. Xorshift32I a -> Int -> a
evalXorshift32I m n = fst (runXorshift32I m n)

execXorshift32I :: forall s a. Xorshift32I a -> Int -> Int
execXorshift32I m n = snd (runXorshift32I m n)
