{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions to generate rules from cabal files
module Cabal2Build.RuleGenerator
  ( generateRulesForCabalFile
  -- * Exported for tests
  , findModulePath
  ) where

import Data.List (intersperse)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Distribution.Compiler as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.ComponentRequestedSpec as Cabal
import qualified Distribution.Types.ExeDependency as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.System as Cabal
import qualified Distribution.Verbosity as Verbosity
import Path (Path, Rel, Dir, File)
import qualified Path as Path
import qualified Path.IO as Path
import Cabal2Build.Rules
import System.FilePath (dropExtension)


generateRulesForCabalFile :: Path b File -> IO [RuleInfo]
generateRulesForCabalFile cabalFilePath = do
  pd <- readCabalFile cabalFilePath
  let library = Cabal.library pd
      executables = Cabal.executables pd
      testsuites = Cabal.testSuites pd
      pkgId = Cabal.package pd
      dataFiles = Cabal.dataFiles pd
  mlibraryRule <-
    traverse (generateLibraryRule cabalFilePath pkgId dataFiles) library
  executablesRules <-
    traverse (generateBinaryRule cabalFilePath pkgId) executables
  testSuiteRules <-
    traverse (generateTestSuiteRule cabalFilePath pkgId) testsuites
  return $ catMaybes $
    maybeToList mlibraryRule ++ executablesRules ++ testSuiteRules

generateLibraryRule
  :: Path b File
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.Library
  -> IO (Maybe RuleInfo)
generateLibraryRule cabalFilePath pkgId dataFiles lib = do
  let pkgName = pkgNameToText $ Cabal.pkgName pkgId
      exposedModules = map Cabal.toFilePath $ Cabal.exposedModules lib
      buildInfo = Cabal.libBuildInfo lib
  generateRule
    cabalFilePath
    pkgId
    dataFiles
    buildInfo
    exposedModules
    LIB
    pkgName

generateBinaryRule
  :: Path b File
  -> Cabal.PackageIdentifier
  -> Cabal.Executable
  -> IO (Maybe RuleInfo)
generateBinaryRule cabalFilePath pkgId executable = do
  let pkgName = pkgNameToText $ Cabal.pkgName pkgId
      exeName = Text.pack $ Cabal.unUnqualComponentName $ Cabal.exeName executable
      targetName =
        if exeName == pkgName then
          exeName <> "-binary"
        else
          exeName
      buildInfo = Cabal.buildInfo executable
      mainis = [dropExtension (Cabal.modulePath executable)]
  generateRule
    cabalFilePath
    pkgId
    []
    buildInfo
    mainis
    EXE
    targetName

generateTestSuiteRule
  :: Path b File
  -> Cabal.PackageIdentifier
  -> Cabal.TestSuite
  -> IO (Maybe RuleInfo)
generateTestSuiteRule cabalFilePath pkgId testsuite = do
  let testName = Text.pack $ Cabal.unUnqualComponentName $ Cabal.testName testsuite
      buildInfo = Cabal.testBuildInfo testsuite
      mainis = [ dropExtension path
               | Cabal.TestSuiteExeV10 _ path <- [Cabal.testInterface testsuite]
               ]
  generateRule
    cabalFilePath
    pkgId
    []
    buildInfo
    mainis
    TEST
    testName

generateRule
  :: Path b File
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.BuildInfo
  -> [FilePath]
  -> ComponentType
  -> Text
  -> IO (Maybe RuleInfo)
generateRule _ _ _ bi _ _ _ | not (Cabal.buildable bi) = return Nothing
generateRule cabalFilePath pkgId dataFiles bi someModules ctype attrName = do
  let pkgName = pkgNameToText $ Cabal.pkgName pkgId
      pkgVersion = Text.pack $ Cabal.prettyShow $ Cabal.pkgVersion pkgId
      versionMacro =
        "-DVERSION_" <> Text.replace "-" "_" pkgName <> "=" <> Text.pack (show pkgVersion)
      otherModules = map Cabal.toFilePath (Cabal.otherModules bi)
  hsSourceDirs <- mapM Path.parseRelDir (Cabal.hsSourceDirs bi)
  someModulePaths <- findModulePaths attrName cabalFilePath hsSourceDirs someModules
  otherModulePaths <- findModulePaths attrName cabalFilePath hsSourceDirs otherModules
  return $ Just $ RuleInfo
        { kind = componentTypeToRuleName ctype
        , name = attrName
        , deps = depPackageNames bi
        , compilerFlags = versionMacro : optionsFromBuildInfo bi
        , extraLibraries = map Text.pack $ Cabal.extraLibs bi
        , tools = map toToolName $ Cabal.buildToolDepends bi
        , attrs =
            [ ("version", TextValue pkgVersion)
            , ("srcs", StringListValue $ map pathToText $ someModulePaths ++ otherModulePaths)
            ] ++
            [ ("hidden_modules", StringListValue xs)
            | Just xs <- [hidden_modules]
            ] ++
            [ ("data", StringListValue $ map Text.pack dataFiles)
            | not (null dataFiles)
            ]
        }
  where
    pathToText = Text.pack . Path.toFilePath

    hidden_modules = case ctype of
      LIB -> Just [ qualifiedModulePath m | m <- Cabal.otherModules bi ]
      _ -> Nothing

    qualifiedModulePath = mconcat . intersperse "." . map Text.pack . Cabal.components

    toToolName (Cabal.ExeDependency pkg exe _) =
      ToolName (pkgNameToText pkg) (Text.pack $ Cabal.unUnqualComponentName exe)


componentTypeToRuleName :: ComponentType -> Text
componentTypeToRuleName = \case
  BENCH -> error "unimplemented"
  EXE -> "haskell_binary"
  LIB -> "haskell_library"
  TEST -> "haskell_test"

-- | @findModulePaths componentName cabalFilePath hsSourceDirs someModules@
--
-- Finds out which files define the given modules under the directory where
-- the cabal file is.
--
-- @componentName@ is used for error reporting only.
--
findModulePaths
  :: Text -> Path b File -> [Path Rel Dir] -> [FilePath] -> IO [Path Rel File]
findModulePaths componentName cabalFilePath hsSourceDirs moduleNames = do
  modulesAsPaths <- mapM Path.parseRelFile moduleNames
  mapM findModule modulesAsPaths
  where
    findModule :: Path Rel File -> IO (Path Rel File)
    findModule modulePath = do
      let cabalDir = Path.parent cabalFilePath
          raiseError = error $ concat
            [ "Could not resolve "
            , show (Path.toFilePath modulePath)
            , " in "
            , show cabalFilePath
            , ":"
            , show componentName
            ]
      maybePath <- findModulePath cabalDir hsSourceDirs modulePath
      maybe raiseError return maybePath

depPackageNames :: Cabal.BuildInfo -> [Text]
depPackageNames =
  map (pkgNameToText . Cabal.depPkgName) . Cabal.targetBuildDepends

-- | @findModulePath parentDir hsSourceDirs modulePaths@ finds
-- the paths of the modules, relative to @hsSourceDirs@.
--
-- The input module path must be relative to some of the directories in
-- @hsSourceDirs@ and must not include an extension. The output of
-- this function will include the actual extension and is relative
-- to @parentDir@.
--
-- The directories in @hsSourceDirs@ must be relative to @parentDir@.
findModulePath :: Path b Dir -> [Path Rel Dir] -> Path Rel File -> IO (Maybe (Path Rel File))
findModulePath parentDir hsSourceDirs modPath =
  case hsSourceDirs of
    [] -> return Nothing
    srcDir:otherDirs -> do
      modulePath <- Path.parseRelFile (Path.toFilePath modPath)
      let fullModulePath = parentDir Path.</> srcDir Path.</> modulePath
      findExtension [".hs"] fullModulePath >>= \case
        Nothing -> findModulePath parentDir otherDirs modPath
        Just ext -> Just <$> Path.addExtension ext (srcDir Path.</> modulePath)
  where
    findExtension [] _ = return Nothing
    findExtension (ext:exts) p = do
      exists <- Path.addExtension ext p >>= Path.doesFileExist
      if exists then return (Just ext)
      else findExtension exts p

pkgNameToText :: Cabal.PackageName -> Text
pkgNameToText = Text.pack . Cabal.unPackageName

-- | Extracts ghc-options and language extensions and returns
-- them as flags for ghc.
optionsFromBuildInfo :: Cabal.BuildInfo -> [Text]
optionsFromBuildInfo bi =
  map (("-X" <>) . Text.pack . Cabal.prettyShow) (Cabal.defaultExtensions bi)
  ++ map Text.pack ghcOptions
  where
    ghcOptions =
      Cabal.cppOptions bi ++
      Cabal.ldOptions bi ++
      concat [xs | (Cabal.GHC, xs) <- Cabal.perCompilerFlavorToList (Cabal.options bi)]

readCabalFile :: Path b File -> IO Cabal.PackageDescription
readCabalFile cabalFilePath = do
  let cabalFile = Path.toFilePath cabalFilePath
  genericPkg <- Cabal.readGenericPackageDescription Verbosity.normal cabalFile
  let flags = mempty
      componentSpec = Cabal.ComponentRequestedSpec
        { Cabal.testsRequested = True
        , Cabal.benchmarksRequested = True
        }
      satisfiableDep = const True
      platform = Cabal.Platform Cabal.buildArch Cabal.buildOS
      ghcVersion = Cabal.mkVersion
        [ div __GLASGOW_HASKELL__ 100
        , mod __GLASGOW_HASKELL__ 10
        , __GLASGOW_HASKELL_PATCHLEVEL1__
        ]
      compilerInfo =
        Cabal.unknownCompilerInfo
          (Cabal.CompilerId Cabal.GHC ghcVersion)
          Cabal.NoAbiTag
  case Cabal.finalizePD
         flags
         componentSpec
         satisfiableDep
         platform
         compilerInfo
         []
         genericPkg of
    Left unresolvedDeps -> error $ concat
      [ "Could not resolve dependencies of "
      , cabalFile
      , ": "
      , show unresolvedDeps
      ]
    Right (pd, _) -> return pd
