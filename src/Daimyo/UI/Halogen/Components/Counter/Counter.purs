module Daimyo.UI.Halogen.Components.Counter (
  uiComponentCounterMain
) where

import Prelude

import Control.Monad.Aff (Aff(), runAff, later', launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Data.Const (Const())
import Data.Void (Void())

import Network.HTTP.Affjax (AJAX())

import Halogen
import Halogen.Util (appendToBody)

import Daimyo.UI.Halogen.Components.Counter.Model
import Daimyo.UI.Halogen.Components.Counter.Component.List
import Daimyo.UI.Halogen.Components.Counter.Component.Counter
import Daimyo.UI.Halogen.Components.Counter.Shared

-- type InstalledComponentP s s' f f' g o o' p p' =
--  ComponentP (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g o p'
--
-- data ChildF p f i = ChildF p (f i)
--
-- install' :: forall s s' f f' g o' p p'. (Plus g, Ord p)
--          => ParentComponentP s s' f f' g (ChildF p f') o' p p'
--          -> (p -> ComponentStateP s' f' g o' p')
--          -> InstalledComponentP s s' f f' g (ChildF p f') o' p p'
--
-- newtype ComponentP s f g o p = Component
--  { render :: State s (HTML p (f Unit))
--  , eval   :: Eval f s f g
--  , peek   :: Peek s f g o
--  }
--
ui :: forall p. InstalledComponentP State Counter ListInput CounterInput CounterEffects (ChildF CounterPlaceholder CounterInput) (Const Void) CounterPlaceholder p
ui = install' list mkCounter

uiComponentCounterMain :: Eff (HalogenEffects (ajax :: AJAX)) Unit
uiComponentCounterMain = do
  launchAff $ do
    -- runUI :: forall eff s f o. ComponentP s f (Aff (HalogenEffects eff)) o Void -> s ->
    --          Aff (HalogenEffects eff) { node :: HTMLElement, driver :: Driver f eff }
    --
    -- installedState :: forall s s' f' g o' p p'. (Ord p) => s -> InstalledStateP s s' f' g o' p p'
    --
    app <- runUI ui (installedState initialState)

    -- appendToBody :: forall m eff. (MonadEff (dom :: DOM | eff) m) => HTMLElement -> m Unit
    --
    appendToBody app.node

    -- type Driver f eff = Natural f (Aff (HalogenEffects eff))
    -- action :: forall f g. (Inject f g) => (Unit -> f Unit) -> g Unit
    app.driver (action ListPing)
