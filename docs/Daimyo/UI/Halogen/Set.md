## Module Daimyo.UI.Halogen.Set

#### `AppSet`

``` purescript
data AppSet a
  = AppSet (Set a) (Maybe String)
```

#### `Input`

``` purescript
data Input a
  = OpAddToSet a
  | OpClearSet
  | OpNop
```


