{-# LANGUAGE OverloadedStrings #-}
module Cabal2Build.RuleGeneratorSpec where

import Test.Hspec
import Cabal2Build.RuleGenerator (findModulePath)
import qualified Path
import qualified Path.IO as Path

spec_toLibraryTarget :: Spec
spec_toLibraryTarget = do
  describe "RuleGenerator.findModulePath" $
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
