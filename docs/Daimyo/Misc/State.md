## Module Daimyo.Misc.State

#### `Counter`

``` purescript
type Counter = Int
```

#### `CounterState`

``` purescript
type CounterState a = State Counter a
```

#### `incr`

``` purescript
incr :: forall eff a. CounterState Counter
```

#### `decr`

``` purescript
decr :: forall eff a. CounterState Counter
```

#### `getr`

``` purescript
getr :: forall eff a. CounterState Counter
```

#### `runCounter`

``` purescript
runCounter :: Tuple Counter Counter
```

#### `CRec`

``` purescript
data CRec
  = CRec { counter :: Counter }
```

#### `CRecState`

``` purescript
type CRecState a = State CRec a
```

#### `crecGetCounter`

``` purescript
crecGetCounter :: CRec -> Counter
```

#### `crecPlus1`

``` purescript
crecPlus1 :: CRec -> CRec
```

#### `crecMinus1`

``` purescript
crecMinus1 :: CRec -> CRec
```

#### `crecGet`

``` purescript
crecGet :: forall eff a. CRecState Counter
```

#### `crecIncr`

``` purescript
crecIncr :: forall eff a. CRecState Counter
```

#### `crecDecr`

``` purescript
crecDecr :: forall eff a. CRecState Counter
```

#### `runCRec`

``` purescript
runCRec :: Counter
```


