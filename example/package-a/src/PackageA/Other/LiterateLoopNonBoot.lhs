\begin{code}
module PackageA.Other.LiterateLoopNonBoot where

import {-# SOURCE #-} PackageA.Other.LiterateLoopBoot(LiterateLoopBoot(..))

data LiterateLoopNonBoot = MkLiterateLoopNonBoot Int

g :: LiterateLoopBoot -> LiterateLoopNonBoot
g (MkLiterateLoopBoot x) = MkLiterateLoopNonBoot x
\end{code}
