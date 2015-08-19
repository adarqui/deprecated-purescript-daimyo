module Daimyo.UI.Halogen.Components.Counter.Shared (
  CounterEffects (..)
) where

import Prelude
import Halogen
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX())

type CounterEffects = Aff (HalogenEffects (ajax :: AJAX))
