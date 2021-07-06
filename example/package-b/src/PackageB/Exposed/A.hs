module PackageB.Exposed.A where

import PackageB.Other.B()

f :: forall a. a -> a
f x = x
