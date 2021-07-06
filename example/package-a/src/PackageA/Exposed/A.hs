module PackageA.Exposed.A where

import PackageA.Other.B(T(T))

instance Show T where
  show T = "T"
