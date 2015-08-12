## Module Daimyo.Data.Map

#### `toArray`

``` purescript
toArray :: forall k v. Map k v -> Array (Tuple k v)
```

#### `elems`

``` purescript
elems :: forall k v. Map k v -> List v
```

#### `indices`

``` purescript
indices :: forall k v. Map k v -> List k
```

#### `filterElems`

``` purescript
filterElems :: forall k v. (v -> Boolean) -> Map k v -> List (Tuple k v)
```

#### `filterElems'`

``` purescript
filterElems' :: forall k v. (v -> Boolean) -> Map k v -> List v
```

#### `filterIndices`

``` purescript
filterIndices :: forall k v. (k -> Boolean) -> Map k v -> List (Tuple k v)
```

#### `filterIndices'`

``` purescript
filterIndices' :: forall k v. (k -> Boolean) -> Map k v -> List v
```

#### `filter`

``` purescript
filter :: forall k v. (k -> v -> Boolean) -> Map k v -> List (Tuple k v)
```

#### `filter'`

``` purescript
filter' :: forall k v. (k -> v -> Boolean) -> Map k v -> List v
```


