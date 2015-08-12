## Module Daimyo.Control.Monad.List

#### `mapM`

``` purescript
mapM :: forall eff m a b. (Monad m, Applicative m) => (a -> m b) -> List a -> m (List b)
```

#### `mapM_`

``` purescript
mapM_ :: forall eff m a b. (Monad m, Applicative m) => (a -> m b) -> List a -> m Unit
```

#### `replicateM_`

``` purescript
replicateM_ :: forall eff m a. (Monad m, Applicative m) => Int -> m a -> m Unit
```


