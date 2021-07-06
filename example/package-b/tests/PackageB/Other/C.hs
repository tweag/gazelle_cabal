module PackageB.Other.C where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test_dummy :: TestTree
test_dummy = testCase "dummy test" $
    return ()
