module Daimyo.UI.Halogen.Components.Counter.Model (
  CounterId (..),
  Counter (..),
  State (..),
  initialState
) where

import Prelude

type CounterId = Int

type Counter = { counter :: Int }

type State = { counters :: Array CounterId, index :: CounterId }

initialState :: State
initialState = { counters: [], index: 1 }
