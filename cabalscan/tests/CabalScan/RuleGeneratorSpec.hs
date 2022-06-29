{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalScan.RuleGeneratorSpec where

import Test.Hspec
import CabalScan.RuleGenerator (findModulePaths)
import qualified System.FilePath as Path
import qualified System.Directory as Path
import qualified System.IO.Temp as Temp

spec_toLibraryTarget :: Spec
spec_toLibraryTarget = do
  describe "RuleGenerator.findModulePaths" $ do
    it "should find modules in multiple directories" $ do
      cwd <- Path.getCurrentDirectory
      Temp.withTempDirectory cwd "findModulePaths" $ \dir -> do
        let aRel = "a"
        Path.createDirectory (dir Path.</> aRel)
        let bRel = "b"
        Path.createDirectory (dir Path.</> bRel)
        let modRel = "A/B/C"
        let foundModulePath = bRel Path.</> modRel Path.<.> ".hs"
        let expected = [foundModulePath]
        Path.createDirectoryIfMissing True $ Path.takeDirectory (dir Path.</> bRel Path.</> modRel)

        writeFile (dir Path.</> foundModulePath) ""
        findModulePaths dir [aRel, bRel] modRel
          `shouldMatchListIO` expected

    it "should find hs-boot files" $ do
      cwd <- Path.getCurrentDirectory
      Temp.withTempDirectory cwd "findModulePaths" $ \dir -> do
        let bRel = "b"
        Path.createDirectory (dir Path.</> bRel)
        let modRel = "A/B/C"
        let foundModulePath = bRel Path.</> modRel Path.<.> ".hs"
        let foundBootPath = bRel Path.</> modRel Path.<.> ".hs-boot"
        let expected = [foundModulePath, foundBootPath]
        Path.createDirectoryIfMissing True $ Path.takeDirectory (dir Path.</> bRel Path.</> modRel)

        writeFile (dir Path.</> foundModulePath) ""
        writeFile (dir Path.</> foundBootPath) ""
        findModulePaths dir [bRel] modRel
          `shouldMatchListIO` expected

    it "should find lhs-boot files" $ do
      cwd <- Path.getCurrentDirectory
      Temp.withTempDirectory cwd "findModulePaths" $ \dir -> do
        let bRel = "b"
        Path.createDirectory (dir Path.</> bRel)
        let modRel = "A/B/C"
        let foundModulePath = bRel Path.</> modRel Path.<.> ".lhs"
        let foundBootPath = bRel Path.</> modRel Path.<.> ".lhs-boot"
        let expected = [foundModulePath, foundBootPath]
        Path.createDirectoryIfMissing True $ Path.takeDirectory (dir Path.</> bRel Path.</> modRel)

        writeFile (dir Path.</> foundModulePath) ""
        writeFile (dir Path.</> foundBootPath) ""
        findModulePaths dir [bRel] modRel
          `shouldMatchListIO` expected

-- | @m \`shouldMatchListIO\` ys@ sets the expectation that @m@ returns a list with the same
-- elements that @ys@ has, possibly in another order.
shouldMatchListIO :: (HasCallStack, Show a, Eq a) => IO [a] -> [a] -> Expectation
action `shouldMatchListIO` expected = action >>= (`shouldMatchList` expected)
