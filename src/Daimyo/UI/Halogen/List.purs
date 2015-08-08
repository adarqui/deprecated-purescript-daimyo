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

import Control.Monad.Aff

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

import Routing

import Daimyo.UI.Shared
import Daimyo.Control.Monad
import Daimyo.Data.Array
import Daimyo.Data.List

data AppList = AppList (L.List String) (Maybe String) Mode

data Mode
  = ModeNormal
  | ModeReverse
  | ModeSort
  | ModeNub

data Input
  = OpAddToList String
  | OpClearList
  | OpClearInput
  | OpSetInput String
  | OpSetMode Mode
  | OpNoOp

instance modeEq :: Eq Mode where
  eq ModeNormal  ModeNormal  = true
  eq ModeReverse ModeReverse = true
  eq ModeSort    ModeSort    = true
  eq ModeNub     ModeNub     = true
  eq _           _           = false

-- | ui
--
ui :: forall eff. Component (E.Event (HalogenEffects eff)) Input Input
ui = render <$> stateful (AppList L.Nil Nothing ModeNormal) update
  where
  render :: AppList -> H.HTML (E.Event (HalogenEffects eff) Input)
  render (AppList list inp mode) = appLayout
    where
    appLayout =
      H.section [] [
        H.header [] [
          H.h1_ [H.text "List"],
          H.button_ [H.a [A.href "#normal"] [H.text "normal"]],
          H.button_ [H.a [A.href "#reverse"] [H.text "reverse"]],
          H.button_ [H.a [A.href "#sort"] [H.text "sort"]],
          H.button_ [H.a [A.href "#nub"] [H.text "nub"]],
          H.br_ [],
          H.input [
            A.placeholder "type something",
            maybe (A.value "") A.value inp,
            A.onValueChanged (\x -> pure (handleNewValue x))
          ] [],
          H.p_ [
            H.ul_ $ listToArray $ map (\s -> H.li_ [H.text s]) $ displayList list
          ]
        ]
      ]

    displayList l
      | mode == ModeReverse = L.reverse l
      | mode == ModeSort    = L.sort l
      | mode == ModeNub     = L.nub l
      | otherwise           = l

  update :: AppList -> Input -> AppList
  update st OpNoOp                          = st
  update (AppList list s m) OpClearList     = AppList L.Nil s m
  update (AppList list _ m) OpClearInput    = AppList list Nothing m
  update (AppList list _ m) (OpSetInput s)  = AppList list (Just s) m
  update (AppList list _ m) (OpAddToList s) = AppList (list `L.snoc` s) (Just s) m
  update (AppList list s m) (OpSetMode m')  = AppList list s m'

handleNewValue :: forall eff. String -> E.Event (HalogenEffects eff) Input
handleNewValue s = E.yield (OpAddToList s) `E.andThen` const (return OpClearInput)

handleModeChange "reverse" = ModeReverse
handleModeChange "sort"    = ModeSort
handleModeChange "nub"     = ModeNub
handleModeChange _         = ModeNormal

uiHalogenListMain = do
  Tuple node driver <- runUI ui
  appendToBody node
  hashChanged (\from to -> do
              runAff throwException driver (return (OpSetMode $ handleModeChange to)))
