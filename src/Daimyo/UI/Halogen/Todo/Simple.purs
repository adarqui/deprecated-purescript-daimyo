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

data AppState = AppState TodoApp (Maybe String) TodoView

data Input
  = OpListTodos (Array Todo)
  | OpAddTodo Todo
  | OpRemoveTodo TodoId
  | OpClearTodos
  | OpClearCompletedTodos
  | OpClearInput
  | OpSetView TodoView
  | OpNop
  | OpBusy

data TodoView
  = ViewAll
  | ViewActive
  | ViewCompleted

-- | ui
--
ui :: forall eff. Component (E.Event (HalogenEffects (ajax :: AJAX | eff))) Input Input
ui = render <$> stateful (AppState newTodoApp Nothing ViewAll) update
  where
  render :: AppState -> H.HTML (E.Event (HalogenEffects (ajax :: AJAX | eff)) Input)
  render (AppState app inp view) = appLayout
    where
    appLayout =
      H.section [class_ "todoapp"] [
        H.header [class_ "header"] [
          H.h1_ [H.text "todos"],
          H.input [
            class_ "new-todo",
            A.placeholder "What needs to be done?",
            maybe (A.value "") A.value inp,
            A.onValueChanged (\x -> pure (handleNewTodo x))
          ] []
        ],
        H.section [class_ "main"] [
          H.input [class_ "toggle-all", A.type_ "checkbox"] [H.label_ [H.text "Mark all as complete"]],
          H.ul [class_ "todo-list"] $ map todoListItem (map snd $ M.toArray $ todoAppTodos app),
          H.footer [class_ "footer"] [
            H.span [class_ "todo-count"] [H.strong_ [H.text $ show $ todosActiveLength (map snd $ M.toArray $ todoAppTodos app)], H.text " items left"],
            H.ul [class_ "filters"] [
              H.li_ [H.a [A.href "#"] [H.text "all"]],
              H.li_ [H.a [A.href "#active"] [H.text "Active"]],
              H.li_ [H.a [A.href "#completed"] [H.text "Completed"]]
            ],
            H.button [class_ "clear-completed", A.onClick (\_ -> pure (handleClearCompleted $ map snd $ M.toArray $ todoAppTodos app))] [H.text "Clear completed"]
          ]
        ],
        H.footer [class_ "info"] [
          H.p_ [H.text "Double-click to edit a todo"],
          H.p_ [H.text "Created by ", H.a [A.href "https://github.com/adarqui/"] [H.text "adarqui"]],
          H.p_ [H.text "Part of ", H.a [A.href "http://todomvc.com"] [H.text "TodoMVC"]]
        ]
      ]

  todoListItem (Todo{todoId=tid, todoTitle=title, todoState=state}) =
    H.li [if state == Completed then class_ "completed" else class_ "active"] [
      H.div [class_ "view"] [
        H.input [class_ "toggle", A.type_ "checkbox", A.checked (state == Completed)] [],
        H.label_ [H.text title],
        H.button [class_ "destroy", A.onClick (\_ -> pure (handleRemoveTodo tid))] []
      ],
      H.input [class_ "edit", A.value title] []
    ]

  update :: AppState -> Input -> AppState
  update (AppState app inp view) (OpListTodos xs)   = AppState (execState (clearTodos >> mapM addTodoDirectly xs) app) inp view
  update (AppState app inp view) (OpAddTodo todo)   = AppState (execState (addTodoDirectly todo) app) inp view
  update (AppState app inp view) (OpRemoveTodo tid) = AppState (execState (removeTodo tid) app) inp view
  update (AppState app inp view) OpClearTodos       = AppState (execState (clearTodos) app) inp view
  update (AppState app _ view)   OpClearInput       = AppState app Nothing view
  update (AppState app inp view) (OpSetView view')  = AppState app inp view'
  update st OpNop                                   = st
  update st OpBusy                                  = st

todosActiveLength :: Array Todo -> Int
todosActiveLength = length <<< todosActive

todosActive :: Array Todo -> Array Todo
todosActive todos = filter (\(Todo{ todoState = state}) -> state == Active) todos

handleListTodos :: forall eff. E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleListTodos = E.yield OpBusy `E.andThen` \_ -> E.async affListTodos

handleNewTodo :: forall eff. String -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleNewTodo s = E.yield OpClearInput `E.andThen` \_ -> handleAddTodo $ defaultTodo s

handleAddTodo :: forall eff. Todo -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleAddTodo todo = E.yield OpClearInput `E.andThen` \_ -> E.yield OpBusy `E.andThen` \_ -> E.async (affAddTodo todo)

handleRemoveTodo :: forall eff. TodoId -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleRemoveTodo tid = E.yield OpBusy `E.andThen` \_ -> E.async (affRemoveTodo tid)

handleClearCompleted :: forall eff. Array Todo -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleClearCompleted todos = go (filter (\(Todo todo) -> todo.todoState == Completed) todos)
--  _ <- mapM_ (\(Todo todo) -> handleRemoveTodo (todo.todoId)) todos
--  return OpNop
  where
  go xs = do
    case (uncons xs) of
         Nothing                          -> return OpNop
         Just { head: (Todo h), tail: t } -> do
           E.async (affRemoveTodo h.todoId) `E.andThen` \_ -> handleClearCompleted t

affListTodos = do
  res <- get "/applications/simple/todos"
  liftEff $ log res.response
  let todos = decode res.response :: Maybe (Array Todo)
  return $ OpListTodos (fromMaybe [] todos)

affAddTodo todo = do
  res <- affjax $ defaultRequest { method = POST, url = "/applications/simple/todos", content = Just (encode (todo :: Todo)), headers = [ContentType applicationJSON] }
  liftEff $ log res.response
  let todo' = decode res.response :: Maybe Todo
  return $ case todo' of
                Nothing   -> OpNop
                Just v    -> OpAddTodo v

affRemoveTodo tid = do
  res <- delete ("/applications/simple/todos/" ++ show (tid :: TodoId))
  liftEff $ log res.response
  let tid = decode res.response :: Maybe TodoId
  return $ case tid of
                Nothing   -> OpNop
                Just tid' -> OpRemoveTodo tid'

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
