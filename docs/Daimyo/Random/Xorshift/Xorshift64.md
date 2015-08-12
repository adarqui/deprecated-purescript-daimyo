## Module Daimyo.Random.Xorshift.Xorshift64

#### `Xorshift64`

``` purescript
data Xorshift64
  = Xorshift64 Int
```

#### `State64`

``` purescript
type State64 = State Xorshift64 Int
```

#### `xorshift64`

``` purescript
xorshift64 :: State64
```

#### `xorshifts64`

``` purescript
xorshifts64 :: Xorshift64 -> Int -> List Int
```

#### `seed64`

``` purescript
seed64 :: Xorshift64
```


