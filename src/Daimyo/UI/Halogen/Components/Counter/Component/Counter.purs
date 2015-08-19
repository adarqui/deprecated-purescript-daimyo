module Daimyo.UI.Halogen.Components.Counter.Component.Counter where

import Prelude

import Data.Functor (($>))
import Data.Tuple (Tuple(..))

import Control.Monad.Aff (Aff())
import Control.Monad.Free (Free(), liftFI)
import Network.HTTP.Affjax (AJAX(), get)

import Halogen
import Halogen.Query.StateF (modify, gets)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

import Daimyo.UI.Halogen.Components.Counter.Model
import Daimyo.UI.Halogen.Components.Counter.Shared

import Data.String

-- | The counter component query algebra.
data CounterInput a
  = CounterIncr a

-- | The placeholder used when installing counter components into a parent
-- | component.
data CounterPlaceholder = CounterPlaceholder CounterId

instance eqCounterPlaceholder :: Eq CounterPlaceholder where
  eq (CounterPlaceholder x) (CounterPlaceholder y) = x == y

instance ordCounterPlaceholder :: Ord CounterPlaceholder where
  compare (CounterPlaceholder x) (CounterPlaceholder y) = compare x y

-- | Creates a `ComponentState` entry based on a `CounterPlaceholder`, used to
-- | install counter components into a parent component.
mkCounter :: forall eff p. CounterPlaceholder -> ComponentState Counter CounterInput (Aff (HalogenEffects (ajax :: AJAX | eff))) p
mkCounter (CounterPlaceholder _) = Tuple counter { counter: 0 }

-- | The counter component definition.
counter :: forall eff p. Component Counter CounterInput (Aff (HalogenEffects (ajax :: AJAX | eff))) p
counter = component render eval
  where

  render :: Render Counter CounterInput p
  render st = H.p_ [H.text "hi"]

  eval :: Eval CounterInput Counter CounterInput (Aff (HalogenEffects (ajax :: AJAX | eff)))
  eval (CounterIncr next) = do
    r <- liftFI $ get "hi" >>= \res -> return (trim res.response)
    pure next
