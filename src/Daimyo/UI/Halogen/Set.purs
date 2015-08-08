module Daimyo.UI.Halogen.Set (
  uiHalogenSetMain
) where

import Prelude
import Data.Maybe
import Data.Tuple

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Control.Alt
import Control.Bind
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)

import Control.Monad.State
import Control.Monad.State.Trans

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Types as T

import qualified Data.Set as S

import Daimyo.UI.Shared

data AppSet = AppSet (S.Set String) (Maybe String)

data Input
  = OpAddToSet String
  | OpClearSet
  | OpNoOp

-- | ui
--
ui :: forall eff. Component (E.Event (HalogenEffects eff)) Input Input
ui = render <$> stateful (AppSet S.empty Nothing) update
--  r <- stateful (AppSet S.empty Nothing) update
--  render r
  where
  render :: AppSet -> H.HTML (E.Event (HalogenEffects eff) Input)
  render (AppSet app inp) = appLayout
    where
    appLayout =
      H.section [] [
        H.header [] [
          H.h1_ [H.text "List"],
          H.input [
            A.placeholder "value",
            maybe (A.value "") A.value inp,
            A.onValueChanged (\x -> pure (handleNewValue x))] []
        ]
      ]

  update :: AppSet -> Input -> AppSet
  update (AppSet set s) OpClearSet = AppSet set s

handleNewValue :: forall eff. String -> E.Event (HalogenEffects eff) Input
handleNewValue s = return OpNoOp

uiHalogenSetMain = do
  Tuple node driver <- runUI ui
  appendToBody node
