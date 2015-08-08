module Daimyo.UI.Halogen.List (
  uiHalogenListMain
) where

import Prelude
import qualified Data.List as L
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

import Daimyo.UI.Shared
import Daimyo.Control.Monad

data AppList = AppList (L.List String) (Maybe String)

data Input
  = OpAddToList String
  | OpClearList
  | OpClearInput
  | OpSetInput String
  | OpNoOp

-- | ui
--
ui :: forall eff. Component (E.Event (HalogenEffects eff)) Input Input
ui = render <$> stateful (AppList L.Nil Nothing) update
  where
  render :: AppList -> H.HTML (E.Event (HalogenEffects eff) Input)
  render (AppList app inp) = appLayout
    where
    appLayout =
      H.section [] [
        H.header [] [
          H.h1_ [H.text "List"],
          H.input [
            A.placeholder "type something",
            maybe (A.value "") A.value inp,
            A.onValueChanged (\x -> pure (handleNewValue x))
          ] []
        ]
      ]

  update :: AppList -> Input -> AppList
  update st OpNoOp                        = st
  update (AppList list s) OpClearList     = AppList L.Nil s
  update (AppList list _) OpClearInput    = AppList list Nothing
  update (AppList list _) (OpSetInput s) = AppList list (Just s)
  update (AppList list _) (OpAddToList s) = AppList list (Just s)

handleNewValue :: forall eff. String -> E.Event (HalogenEffects eff) Input
handleNewValue s = return OpClearInput

uiHalogenListMain = do
  Tuple node driver <- runUI ui
  appendToBody node
