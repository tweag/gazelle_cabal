{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalScan.RuleGeneratorSpec where

import Test.Hspec
import CabalScan.RuleGenerator (findModulePaths)
import qualified Path
import qualified Path.IO as Path

spec_toLibraryTarget :: Spec
spec_toLibraryTarget = do
  describe "RuleGenerator.findModulePaths" $ do
    it "should find modules in multiple directories" $ do
      cwd <- Path.getCurrentDir
      Path.withTempDir cwd "findModulePaths" $ \dir -> do
        aRel <- Path.parseRelDir "a"
        Path.createDir (dir Path.</> aRel)
        bRel <- Path.parseRelDir "b"
        Path.createDir (dir Path.</> bRel)
        modRel <- Path.parseRelFile "A/B/C"
        foundModulePath <- (bRel Path.</>) <$> Path.addExtension ".hs" modRel
        let expected = [foundModulePath]
        Path.ensureDir $ Path.parent (dir Path.</> bRel Path.</> modRel)

        writeFile (Path.toFilePath (dir Path.</> foundModulePath) ) ""
        findModulePaths dir [aRel, bRel] modRel
          `shouldMatchListIO` expected

    it "should find hs-boot files" $ do
      cwd <- Path.getCurrentDir
      Path.withTempDir cwd "findModulePaths" $ \dir -> do
        bRel <- Path.parseRelDir "b"
        Path.createDir (dir Path.</> bRel)
        modRel <- Path.parseRelFile "A/B/C"
        foundModulePath <- (bRel Path.</>) <$> Path.addExtension ".hs" modRel
        foundBootPath <- (bRel Path.</>) <$> Path.addExtension ".hs-boot" modRel
        let expected = [foundModulePath, foundBootPath]
        Path.ensureDir $ Path.parent (dir Path.</> bRel Path.</> modRel)

        writeFile (Path.toFilePath (dir Path.</> foundModulePath) ) ""
        writeFile (Path.toFilePath (dir Path.</> foundBootPath) ) ""
        findModulePaths dir [bRel] modRel
          `shouldMatchListIO` expected

-- | @m \`shouldMatchListIO\` ys@ sets the expectation that @m@ returns a list with the same
-- elements that @ys@ has, possibly in another order.
shouldMatchListIO :: (HasCallStack, Show a, Eq a) => IO [a] -> [a] -> Expectation
action `shouldMatchListIO` expected = action >>= (`shouldMatchList` expected)
