{-# LANGUAGE OverloadedStrings #-}
module CabalScan.RuleGeneratorSpec where

import Test.Hspec
import CabalScan.RuleGenerator (findModulePath)
import qualified Path
import qualified Path.IO as Path

spec_toLibraryTarget :: Spec
spec_toLibraryTarget = do
  describe "RuleGenerator.findModulePath" $ do
    it "should find modules in multiple directories" $ do
      cwd <- Path.getCurrentDir
      Path.withTempDir cwd "findModulePath" $ \dir -> do
        aRel <- Path.parseRelDir "a"
        Path.createDir (dir Path.</> aRel)
        bRel <- Path.parseRelDir "b"
        Path.createDir (dir Path.</> bRel)
        modRel <- Path.parseRelFile "A/B/C"
        expected <-
          (bRel Path.</>) <$> Path.addExtension ".hs" modRel
        Path.ensureDir $ Path.parent (dir Path.</> bRel Path.</> modRel)

        writeFile (Path.toFilePath (dir Path.</> expected) ) ""
        findModulePath dir [aRel, bRel] modRel
          `shouldReturn` Just expected

    it "should find hs-boot files" $ do
      cwd <- Path.getCurrentDir
      Path.withTempDir cwd "findModulePath" $ \dir -> do
        bRel <- Path.parseRelDir "b"
        Path.createDir (dir Path.</> bRel)
        modRel <- Path.parseRelFile "A/B/C"
        expected <-
          (bRel Path.</>) <$> Path.addExtension ".hs-boot" modRel
        Path.ensureDir $ Path.parent (dir Path.</> bRel Path.</> modRel)

        writeFile (Path.toFilePath (dir Path.</> expected) ) ""
        findModulePath dir [bRel] modRel
          `shouldReturn` Just expected
