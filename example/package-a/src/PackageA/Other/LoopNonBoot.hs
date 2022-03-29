module PackageA.Other.LoopNonBoot where

import {-# SOURCE #-} PackageA.Other.LoopBoot(LoopBoot(..))

data LoopNonBoot = MkLoopNonBoot Int

g :: LoopBoot -> LoopNonBoot
g (MkLoopBoot x) = MkLoopNonBoot x
