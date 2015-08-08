module Daimyo.UI.Shared where

import Prelude

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Types as T

import qualified Halogen.HTML.CSS as CSS

import Control.Bind
import Control.Monad.Eff

-- | appendToBody
--
appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

-- | class_
--
class_ = A.class_ <<< A.className
