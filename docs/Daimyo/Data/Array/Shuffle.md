## Module Daimyo.Data.Array.Shuffle

#### `shuffleEff`

``` purescript
shuffleEff :: forall eff a. (Ord a) => Array a -> Eff (random :: RANDOM | eff) (Array a)
```

#### `shuffleLCG_BSD`

``` purescript
shuffleLCG_BSD :: forall a. (Ord a) => Int -> Array a -> Array a
```

#### `shuffleLCG_MS`

``` purescript
shuffleLCG_MS :: forall a. (Ord a) => Int -> Array a -> Array a
```

#### `pick1_BSD`

``` purescript
pick1_BSD :: forall a. Int -> Array a -> Maybe a
```

#### `pick1_MS`

``` purescript
pick1_MS :: forall a. Int -> Array a -> Maybe a
```


