module PackageD.Core
  ( greet
  ) where

import PackageD.Internal.Base

greet :: String -> String
greet name = baseMessage ++ ", " ++ name ++ "!"
