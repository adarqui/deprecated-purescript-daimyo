## Module Daimyo.Control.Monad.Array

#### `mapM`

``` purescript
mapM :: forall eff m a b. (Monad m, Applicative m) => (a -> m b) -> Array a -> m (Array b)
```

#### `mapM_`

``` purescript
mapM_ :: forall eff m a b. (Monad m, Applicative m) => (a -> m b) -> Array a -> m Unit
```


