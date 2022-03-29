module PackageA.Other.LoopBoot where

import PackageA.Other.LoopNonBoot (LoopNonBoot(..))

data LoopBoot = MkLoopBoot Int

f :: LoopNonBoot -> LoopBoot
f (MkLoopNonBoot x) = MkLoopBoot x
