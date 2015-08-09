module Daimyo.UI.Halogen.Todo.Simple (
  uiHalogenTodoSimpleMain
) where

import Prelude
import Data.Array (filter, length, (:), uncons)
import Data.Tuple
import Data.Maybe
import Data.JSON

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

import qualified Halogen.HTML.CSS as CSS

import Control.Monad.Aff
import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader

import Routing

import Daimyo.Control.Monad
import Daimyo.Applications.Todo.Simple
import Daimyo.UI.Shared
import qualified Daimyo.Data.Map as M
import qualified Data.Map as M

data AppState = AppState TodoApp (Maybe String) TodoView UIMode

data Input
  = OpListTodos (Array Todo)
  | OpAddTodo Todo
  | OpRemoveTodo TodoId
  | OpUpdateTodo TodoId Todo
  | OpClearTodos
  | OpClearInput
  | OpSetView TodoView
  | OpSetMode UIMode
  | OpNop
  | OpBusy

data TodoView
  = ViewAll
  | ViewActive
  | ViewCompleted

data UIMode
  = ModeView
  | ModeEdit TodoId

instance uimodeEq :: Eq UIMode where
  eq ModeView ModeView           = true
  eq (ModeEdit t1) (ModeEdit t2) = t1 == t2
  eq _             _             = false

instance todoviewEq :: Eq TodoView where
  eq ViewAll ViewAll             = true
  eq ViewActive ViewActive       = true
  eq ViewCompleted ViewCompleted = true
  eq _             _             = false

-- | ui
--
ui :: forall eff. Component (E.Event (HalogenEffects (ajax :: AJAX | eff))) Input Input
ui = render <$> stateful (AppState newTodoApp Nothing ViewAll ModeView) update
  where
  render :: AppState -> H.HTML (E.Event (HalogenEffects (ajax :: AJAX | eff)) Input)
  render (AppState app inp view mode) = appLayout
    where
    run commands = evalState commands app
    appLayout =
      H.section [class_ "todoapp"] [
        H.header [class_ "header"] [
          H.h1_ [H.text "todos"],
          H.input [
            class_ "new-todo",
            A.placeholder "What needs to be done?",
            maybe (A.value "") A.value inp,
              A.onValueChanged (pure <<< handleNewTodo)
          ] []
        ],
        H.section [class_ "main"] [
          H.input [class_ "toggle-all", A.type_ "checkbox"] [H.label_ [H.text "Mark all as complete"]],
          H.ul [class_ "todo-list"] $ map todoListItem todosFilter,
          H.footer [class_ "footer"] [
            H.span [class_ "todo-count"] [H.strong_ [H.text $ show $ length $ run listActiveTodos], H.text " items left"],
            H.ul [class_ "filters"] [
              H.li_ [H.a [A.href "#"] [H.text "All"]],
              H.li_ [H.a [A.href "#active"] [H.text "Active"]],
              H.li_ [H.a [A.href "#completed"] [H.text "Completed"]]
            ],
            H.button [class_ "clear-completed", A.onClick (const $ pure (handleClearCompleted $ run listCompletedTodos))] [H.text "Clear completed"]
          ]
        ],
        H.footer [class_ "info"] [
          H.p_ [H.text "Double-click to edit a todo"],
          H.p_ [H.text "Created by ", H.a [A.href "https://github.com/adarqui/"] [H.text "adarqui"]],
          H.p_ [H.text "Part of ", H.a [A.href "http://todomvc.com"] [H.text "TodoMVC"]]
        ]
      ]

    todosFilter :: Array Todo
    todosFilter
      | view == ViewAll       = run listTodos
      | view == ViewActive    = run listActiveTodos
      | view == ViewCompleted = run listCompletedTodos

    todoListItem (todo@Todo{todoId=tid, todoTitle=title, todoState=state}) =
      let v = H.label [A.onClick (const $ pure $ return $ OpSetMode (ModeEdit tid))] [H.text title] in
      H.li [if state == Completed then class_ "completed" else class_ "active"] [
        H.div [class_ "view"] [
          H.input [class_ "toggle", A.type_ "checkbox", A.checked (state == Completed), A.onChange (const $ pure (handleUpdateTodo tid title (toggleTodoState state)))] [],
          case mode of
             ModeView      -> v
             ModeEdit tid' ->
              if tid /= tid'
                 then v
                 else H.input [ class_ "new-todo", A.value title, A.onValueChanged (\x -> pure (handleUpdateTodo tid x state)), A.onFocusOut (const $ pure (return $ OpSetMode ModeView)) ] [],
          H.button [class_ "destroy", A.onClick (const $ pure (handleRemoveTodo tid))] []
        ],
        H.input [class_ "edit", A.value title] []
      ]

  update :: AppState -> Input -> AppState
  update (AppState app inp view mode) (OpListTodos xs)        = AppState (execState (clearTodos >> mapM addTodoDirectly xs) app) inp view mode
  update (AppState app inp view mode) (OpAddTodo todo)        = AppState (execState (addTodoDirectly todo) app) inp view mode
  update (AppState app inp view mode) (OpRemoveTodo tid)      = AppState (execState (removeTodo tid) app) inp view mode
  update (AppState app inp view mode) (OpUpdateTodo tid todo) = AppState (execState (updateTodo tid todo) app) inp view mode
  update (AppState app inp view mode) OpClearTodos            = AppState (execState clearTodos app) inp view mode
  update (AppState app _ view mode)   OpClearInput            = AppState app Nothing view mode
  update (AppState app inp view mode) (OpSetView view')       = AppState app inp view' mode
  update (AppState app inp view _)    (OpSetMode mode)        = AppState app inp view mode
  update st OpNop                                             = st
  update st OpBusy                                            = st

handleListTodos :: forall eff. E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleListTodos = E.yield OpBusy `E.andThen` \_ -> E.async affListTodos

handleNewTodo :: forall eff. String -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleNewTodo s = E.yield OpClearInput `E.andThen` \_ -> handleAddTodo $ defaultTodo s

handleAddTodo :: forall eff. Todo -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleAddTodo todo = E.yield OpClearInput `E.andThen` \_ -> E.yield OpBusy `E.andThen` \_ -> E.async (affAddTodo todo)

handleRemoveTodo :: forall eff. TodoId -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleRemoveTodo tid = E.yield OpBusy `E.andThen` \_ -> E.async (affRemoveTodo tid)

handleUpdateTodo :: forall eff. TodoId -> String -> TodoState -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleUpdateTodo tid title state = E.yield OpBusy `E.andThen` \_ -> E.async (affUpdateTodo (Todo{todoId: tid, todoTitle: title, todoState: state})) `E.andThen` \_ -> return $ OpSetMode ModeView

handleClearCompleted :: forall eff. Array Todo -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleClearCompleted = go --go (filter (\(Todo todo) -> todo.todoState == Completed) todos)
  where
  go xs = do
    case (uncons xs) of
         Nothing                          -> return OpNop
         Just { head: (Todo h), tail: t } -> do
           E.async (affRemoveTodo h.todoId) `E.andThen` \_ -> handleClearCompleted t

affListTodos = do
  res <- get "/applications/simple/todos"
  let todos = decode res.response :: Maybe (Array Todo)
  return $ OpListTodos (fromMaybe [] todos)

affAddTodo todo = do
  res <- affjax $ defaultRequest { method = POST, url = "/applications/simple/todos", content = Just (encode (todo :: Todo)), headers = [ContentType applicationJSON] }
  let todo' = decode res.response :: Maybe Todo
  return $ case todo' of
                Nothing   -> OpNop
                Just v    -> OpAddTodo v

affRemoveTodo tid = do
  res <- delete ("/applications/simple/todos/" ++ show (tid :: TodoId))
  let tid = decode res.response :: Maybe TodoId
  return $ case tid of
                Nothing   -> OpNop
                Just tid' -> OpRemoveTodo tid'

affUpdateTodo todo@Todo{todoId: tid, todoTitle: title, todoState: state} = do
  res <- affjax $ defaultRequest { method = PUT, url = ("/applications/simple/todos/" ++ show tid), content = Just (encode (todo :: Todo)), headers = [ContentType applicationJSON] }
  let todo' = decode res.response :: Maybe Todo
  return $ case todo' of
                Nothing    -> OpNop
                Just todo' -> OpUpdateTodo tid todo'

handleViewChange :: String -> TodoView
handleViewChange "active"    = ViewActive
handleViewChange "completed" = ViewCompleted
handleViewChange _           = ViewAll

uiHalogenTodoSimpleMain = do
  Tuple node driver <- runUI ui
  appendToBody node
  runAff throwException driver affListTodos
  hashChanged (\from to -> do
              runAff throwException driver $ do
                return $ (OpSetView $ handleViewChange to))
