\begin{code}
module PackageA.Other.LiterateLoopBoot where

import PackageA.Other.LiterateLoopNonBoot (LiterateLoopNonBoot(..))

data LiterateLoopBoot = MkLiterateLoopBoot Int

f :: LiterateLoopNonBoot -> LiterateLoopBoot
f (MkLiterateLoopNonBoot x) = MkLiterateLoopBoot x
\end{code}
