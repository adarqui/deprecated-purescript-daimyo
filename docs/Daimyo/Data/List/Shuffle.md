## Module Daimyo.Data.List.Shuffle

#### `shuffleEff`

``` purescript
shuffleEff :: forall eff a. (Ord a) => List a -> Eff (random :: RANDOM | eff) (List a)
```

#### `shuffleLCG_BSD`

``` purescript
shuffleLCG_BSD :: forall a. (Ord a) => Int -> List a -> List a
```

#### `shuffleLCG_MS`

``` purescript
shuffleLCG_MS :: forall a. (Ord a) => Int -> List a -> List a
```

#### `pick1_BSD`

``` purescript
pick1_BSD :: forall a. Int -> List a -> Maybe a
```

#### `pick1_MS`

``` purescript
pick1_MS :: forall a. Int -> List a -> Maybe a
```


