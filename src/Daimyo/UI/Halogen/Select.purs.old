module Daimyo.UI.Halogen.Select (
  uiHalogenSelectMain
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

import Daimyo.UI.Shared

data AppState = AppState Option

data Input
  = OpChangeOption Option
  | OpNop

data Option
  = Compress
  | Decompress
  | Encrypt
  | Decrypt

instance optionShow :: Show Option where
  show Compress   = "compress"
  show Decompress = "decompress"
  show Encrypt    = "encrypt"
  show Decrypt    = "decrypt"

-- | ui
--
ui :: forall eff. Component (E.Event (HalogenEffects eff)) Input Input
ui = render <$> stateful (AppState Compress) update
  where
  render :: forall eff. AppState -> H.HTML (E.Event (HalogenEffects eff) Input)
  render (AppState option) = appLayout
    where
    appLayout =
      H.section [] [
        H.header [] [
          H.h1_ [H.text "Select"],
          H.select [
            A.onValueChanged (pure <<< handleChangeOption)
          ] [
            H.option_ [H.text "compress"],
            H.option_ [H.text "decompress"],
            H.option_ [H.text "encrypt"],
            H.option_ [H.text "decrypt"]
          ],
          H.p_ [displaySelect]
        ]
      ]

    -- | displaySelect
    displaySelect = H.p_ [H.text $ (show option) ++ " is selected"]

  update :: AppState -> Input -> AppState
  update _ (OpChangeOption opt) = AppState opt
  update st OpNop               = st

handleChangeOption :: forall eff. String -> E.Event (HalogenEffects eff) Input
handleChangeOption s = return $ OpChangeOption opt
  where
  opt
    | s == "compress"   = Compress
    | s == "decompress" = Decompress
    | s == "encrypt"    = Encrypt
    | s == "decrypt"    = Decrypt

uiHalogenSelectMain = do
  Tuple node driver <- runUI ui
  appendToBody node
