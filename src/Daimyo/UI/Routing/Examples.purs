module Daimyo.UI.Routing.Examples (
  uiRoutingExamplesMain
) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Alt
import Control.Apply
import Data.List

import Routing
import Routing.Match
import Routing.Match.Class

data RTypes
  = RBool Boolean
  | RString String
  | RNum Number
  | RParam Number String
  | RUnknown

instance rtypesShow :: Show RTypes where
  show (RBool b)    = show b
  show (RString s)  = s
  show (RNum n)     = show n
  show (RParam n p) = show n <> " " <> p
  show RUnknown     = "unknown"

-- class (Alternative f) <= MatchClass f where
--  lit :: String -> f Unit
--  str :: f String
--  param :: String -> f String
--  num :: f Number
--  bool :: f Boolean
--  fail :: forall a. String -> f a
--
-- (<$>) :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
--
-- (<*>) :: f (a -> b) -> f a -> f b
--
-- (*>) :: f a -> f b -> f b
--
-- (<|>) :: forall f a. (Alt f) => f a -> f a -> f a
--

-- | routing
--
-- matches routes:
-- /.../Examples.html#bool/true
-- /.../Examples.html#string/hey
-- /.../Examples.html#number/5.0
-- /.../Examples.html#param/1.0?par=OnePointOh
--
routing :: Match RTypes
routing =
  RBool <$> (lit "bool" *> bool)
  <|>
  RString <$> (lit "string" *> str)
  <|>
  RNum <$> (lit "number" *> num)
  <|>
  RParam <$> (lit "param" *> num) <*> (param "par") -- wrong
  <|>
  pure RUnknown

uiRoutingExamplesMain :: forall eff. Eff (console :: CONSOLE | eff) Unit
uiRoutingExamplesMain = do
  -- foreign import hashChanged :: forall e. (String -> String -> Eff e Unit) -> Eff e Unit
  --
  --  hashChanged $ \old new -> void do
  --    print "hash changed."

  -- matches :: forall e a. Match a -> (Maybe a -> a -> Eff e Unit) -> Eff e Unit
  --
  matches routing $ \old new -> void do
    print $ "matches old: " ++ show old
    print $ "matches new: " ++ show new
