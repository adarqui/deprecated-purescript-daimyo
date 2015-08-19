module Daimyo.UI.Halogen.Components.Counter.Component.List where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff(), runAff, launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (Free(), liftFI)
import Control.Monad.Aff.AVar (AVAR (..))
import Control.Monad.Eff.Exception (EXCEPTION (..), throwException)

import Data.Array (snoc, filter, length)
import Data.Const (Const())
import Data.Functor (($>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Void (Void())

import Halogen
import Halogen.Query.StateF (modify)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader

import Daimyo.UI.Halogen.Components.Counter.Model
import Daimyo.UI.Halogen.Components.Counter.Shared
import Daimyo.UI.Halogen.Components.Counter.Component.Counter

import qualified Data.String as S

-- | The list component query algebra.
data ListInput a
  = ListPing a

-- | The list component definition.
--
-- type ParentComponentP s s' f f' g o o' p p' = ComponentP s f (QueryFP s s' f' g o' p p') o p
--
-- newtype ComponentP s f g o p = Component
--  { render :: State s (HTML p (f Unit))
--  , eval   :: Eval f s f g
--  , peek   :: Peek s f g o
--  }
--
-- | Data type for Halogen components.
-- | - `s` - the type of the component state
-- | - `f` - the component's query algebra
-- | - `g` - the monad handling the component's non-state effects
-- | - `o` - the type of values observable via `peek`, used to allow parent
-- |         components to see queries their children have acted upon.
-- | - `p` - the type of placeholders within the component, used to specify
-- |         "holes" in which child components can be installed.
--
-- type ParentComponentP s s' f f' g o o' p p' =
-- s  = State
-- s' = Counter
-- f  = ListInput
-- f' = CounterInput
-- g  = CounterEffects
-- o  = ChildF CounterPlaceholder CounterInput
-- o' = Const Void
-- p  = CounterPlaceholder
-- p' = p
--

list'1 :: forall p.
  ParentComponentP                           -- ParentComponentP s s' f f' g o o' p p'
    State                                    -- s
    Counter                                  -- s'
    ListInput                                -- f
    CounterInput                             -- f'
    CounterEffects                           -- g
    (ChildF CounterPlaceholder CounterInput) -- o
    (Const Void)                             -- o'
    CounterPlaceholder                       -- p
    p                                        -- p'
list'1 = component' render eval peek
  where
  render :: Render State ListInput CounterPlaceholder
  render st = H.p_ [H.text "yo"]
  eval :: Eval ListInput State ListInput (QueryF State Counter CounterInput CounterEffects CounterPlaceholder p)
  eval (ListPing next) = do
    pure next
  peek :: Peek State ListInput (QueryF State Counter CounterInput CounterEffects CounterPlaceholder p) (ChildF CounterPlaceholder CounterInput)
  peek (ChildF p q) = case q of
    _ -> pure unit
--
-- ComponentP s f (QueryFP s s' f' g o' p p') o p
--
-- list :: forall p.
--   ComponentP State ListInput
--   (QueryFP State Counter CounterInput CounterEffects (Const Void) CounterPlaceholder p)
--   (ChildF CounterPlaceholder CounterInput)
--   CounterPlaceholder
--
-- | An intermediate algebra that component containers "produce" (use as their
-- | `g` type variable).
-- type QueryFP s s' f' g o' p p' = Free (HalogenF (InstalledStateP s s' f' g o' p p') (ChildF p f') g)
-- type QueryF s s' f' g p p' = QueryFP s s' f' g (Const Void) p p'
--
list :: forall p.
  ComponentP State ListInput
  (QueryFP State Counter CounterInput CounterEffects (Const Void) CounterPlaceholder p)
  (ChildF CounterPlaceholder CounterInput)
  CounterPlaceholder
list = component' render eval peek
  where

  render :: Render State ListInput CounterPlaceholder
  render st = H.p_ [H.text "yo"]

  -- type Eval i s f g = Natural i (Free (HalogenF s f g))
  --
  -- type QueryF s s' f' g p p' = QueryFP s s' f' g (Const Void) p p'
  --
  eval :: Eval ListInput State ListInput (QueryF State Counter CounterInput CounterEffects CounterPlaceholder p)

  eval (ListPing next) = do
    r <- liftFI (get "hi" >>= \res -> return (S.trim res.response))
--    counters <- liftFI $ ajaxListTodos
    pure next

  -- type HalogenF s f g = Coproduct (StateF s) (Coproduct (SubscribeF f g) g)
  --
  -- data SubscribeF f g a = Subscribe (EventSource f g) a
  --
  -- type Peek s f g o = forall a. o a -> Free (HalogenF s f g) Unit
  --
  -- data ChildF p f i = ChildF p (f i)
  --
  peek :: Peek State ListInput (QueryF State Counter CounterInput CounterEffects CounterPlaceholder p) (ChildF CounterPlaceholder CounterInput)
  peek (ChildF p q) = case q of
    _ -> pure unit

-- | AJAX: List Todos
ajaxListCounters :: forall eff. Aff (ajax :: AJAX | eff) String
ajaxListCounters = do
  res <- affjax $ defaultRequest { method = GET, url = "/counters/", headers = [ContentType applicationJSON] }
  return res.response
