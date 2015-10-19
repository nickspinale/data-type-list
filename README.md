# data-type-util

Type-level analogs of some basic `Data.*`  modules.

Example:

```haskell
λ> :set -XDataKinds
λ> import Data.Type.List
λ> :t undefined :: Foldr (->) r '[a, b, c]
undefined :: Foldr (->) r '[a, b, c] :: a -> b -> c -> r
```

Created to decouple some of the functionality that will eventually be neaded by the [bigword](https://github.com/nickspinale/bigword) package.
