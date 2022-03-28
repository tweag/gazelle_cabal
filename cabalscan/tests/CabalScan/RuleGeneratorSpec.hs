{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalScan.RuleGeneratorSpec where

import Test.Hspec
import CabalScan.RuleGenerator (FoundModulePath(..), findModulePath)
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
        foundModulePath <- (bRel Path.</>) <$> Path.addExtension ".hs" modRel
        let expected =
              FoundModulePath
                { foundModulePath
                , foundBootPath = Nothing
                }
        Path.ensureDir $ Path.parent (dir Path.</> bRel Path.</> modRel)

        writeFile (Path.toFilePath (dir Path.</> foundModulePath) ) ""
        findModulePath dir [aRel, bRel] modRel
          `shouldReturn` Just expected

    it "should find hs-boot files" $ do
      cwd <- Path.getCurrentDir
      Path.withTempDir cwd "findModulePath" $ \dir -> do
        bRel <- Path.parseRelDir "b"
        Path.createDir (dir Path.</> bRel)
        modRel <- Path.parseRelFile "A/B/C"
        foundModulePath <- (bRel Path.</>) <$> Path.addExtension ".hs" modRel
        foundBootPath <- (bRel Path.</>) <$> Path.addExtension ".hs-boot" modRel
        let expected =
              FoundModulePath
                { foundModulePath
                , foundBootPath = Just foundBootPath
                }
        Path.ensureDir $ Path.parent (dir Path.</> bRel Path.</> modRel)

        writeFile (Path.toFilePath (dir Path.</> foundModulePath) ) ""
        writeFile (Path.toFilePath (dir Path.</> foundBootPath) ) ""
        findModulePath dir [bRel] modRel
          `shouldReturn` Just expected
