## Module Daimyo.Applications.Todo.Simple

#### `TodoId`

``` purescript
type TodoId = Int
```

#### `TodoState`

``` purescript
data TodoState
  = Active
  | Completed
```

##### Instances
``` purescript
instance todoStateEq :: Eq TodoState
instance showTodoState :: Show TodoState
instance todoStateFromJSON :: FromJSON TodoState
instance todoStateToJSON :: ToJSON TodoState
```

#### `Todo`

``` purescript
data Todo
  = Todo { todoId :: TodoId, todoTitle :: String, todoState :: TodoState }
```

##### Instances
``` purescript
instance todoEq :: Eq Todo
instance showTodo :: Show Todo
instance todoFromJSON :: FromJSON Todo
instance todoToJSON :: ToJSON Todo
```

#### `TodoActionRequest`

``` purescript
data TodoActionRequest
  = ReqListTodos
  | ReqAddTodo Todo
  | ReqRemoveTodo TodoId
  | ReqUpdateTodo TodoId Todo
  | ReqFindTodoById TodoId
  | ReqClearTodos
  | ReqNoOp
```

#### `TodoActionResponse`

``` purescript
data TodoActionResponse
  = RespListTodos (Array Todo)
  | RespAddTodo Todo
  | RespRemoveTodo (Maybe TodoId)
  | RespUpdateTodo (Maybe Todo)
  | RespFindTodoById (Maybe Todo)
  | RespClearTodos Int
  | RespNoOp
```

#### `TodoApp`

``` purescript
data TodoApp
  = TodoApp { todoAppCounter :: TodoId, todoAppTodos :: Map TodoId Todo }
```

#### `TodoAppState`

``` purescript
type TodoAppState a = State TodoApp a
```

#### `todoId`

``` purescript
todoId :: Todo -> TodoId
```

#### `todoAppCounter`

``` purescript
todoAppCounter :: TodoApp -> TodoId
```

#### `todoAppTodos`

``` purescript
todoAppTodos :: TodoApp -> Map TodoId Todo
```

#### `newTodoApp`

``` purescript
newTodoApp :: TodoApp
```

#### `listTodos`

``` purescript
listTodos :: forall eff a. TodoAppState (Array Todo)
```

#### `listActiveTodos`

``` purescript
listActiveTodos :: forall eff a. TodoAppState (Array Todo)
```

#### `listCompletedTodos`

``` purescript
listCompletedTodos :: forall eff a. TodoAppState (Array Todo)
```

#### `clearTodos`

``` purescript
clearTodos :: forall eff a. TodoAppState Int
```

#### `addTodo`

``` purescript
addTodo :: forall eff a. Todo -> TodoAppState Todo
```

#### `addTodoDirectly`

``` purescript
addTodoDirectly :: forall eff a. Todo -> TodoAppState Todo
```

#### `removeTodo`

``` purescript
removeTodo :: forall eff a. TodoId -> TodoAppState (Maybe TodoId)
```

#### `updateTodo`

``` purescript
updateTodo :: forall eff a. TodoId -> Todo -> TodoAppState (Maybe Todo)
```

#### `findTodoById`

``` purescript
findTodoById :: forall eff a. TodoId -> TodoAppState (Maybe Todo)
```

#### `toggleTodoState`

``` purescript
toggleTodoState :: TodoState -> TodoState
```

#### `defaultTodo`

``` purescript
defaultTodo :: String -> Todo
```

#### `runTodoGrammar`

``` purescript
runTodoGrammar :: TodoActionRequest -> TodoAppState TodoActionResponse
```

#### `runTodoGrammarDirectly`

``` purescript
runTodoGrammarDirectly :: TodoActionRequest -> TodoAppState TodoActionResponse
```


