{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions to generate rules from Cabal files
module CabalScan.RuleGenerator
  ( generateRulesForCabalFile
  -- * Exported for tests
  , findModulePath
  ) where

import Control.Exception (Exception, throwIO)
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
import CabalScan.Rules
    ( ToolName(ToolName),
      AttrValue(StringListValue, TextValue),
      ImportData(ImportData, deps, compilerFlags, extraLibraries, tools),
      RuleInfo(..),
      ComponentType(..) )
import System.FilePath (dropExtension)


generateRulesForCabalFile :: Path b File -> IO [RuleInfo]
generateRulesForCabalFile cabalFilePath = do
  pd <- readCabalFile cabalFilePath
  let library = Cabal.library pd
      sublibraries = Cabal.subLibraries pd
      executables = Cabal.executables pd
      testsuites = Cabal.testSuites pd
      benchmarks = Cabal.benchmarks pd
      pkgId = Cabal.package pd
      dataFiles = Cabal.dataFiles pd
  mlibraryRule <-
    traverse (generateLibraryRule cabalFilePath pkgId dataFiles) library
  sublibraryRules <-
    traverse (generateLibraryRule cabalFilePath pkgId dataFiles) sublibraries
  executablesRules <-
    traverse (generateBinaryRule cabalFilePath pkgId dataFiles) executables
  testSuiteRules <-
    traverse (generateTestRule cabalFilePath pkgId dataFiles) testsuites
  benchmarkRules <-
    traverse (generateBenchmarkRule cabalFilePath pkgId dataFiles) benchmarks
  return $ catMaybes $
    maybeToList mlibraryRule ++ sublibraryRules ++ executablesRules ++ testSuiteRules ++ benchmarkRules

generateLibraryRule
  :: Path b File
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.Library
  -> IO (Maybe RuleInfo)
generateLibraryRule cabalFilePath pkgId dataFiles lib = do
  let libraryName = obtainLibraryName $ Cabal.libName lib
      exposedModules = map Cabal.toFilePath $ Cabal.exposedModules lib
      buildInfo = Cabal.libBuildInfo lib
  generateRule
    cabalFilePath
    pkgId
    dataFiles
    buildInfo
    exposedModules
    LIB
    libraryName
  where
    obtainLibraryName :: Cabal.LibraryName -> Text
    obtainLibraryName (Cabal.LSubLibName name) = Text.pack . Cabal.unUnqualComponentName $ name
    obtainLibraryName _ = pkgNameToText . Cabal.pkgName $ pkgId

generateBinaryRule
  :: Path b File
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.Executable
  -> IO (Maybe RuleInfo)
generateBinaryRule cabalFilePath pkgId dataFiles executable = do
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
    dataFiles
    buildInfo
    mainis
    EXE
    targetName

generateTestRule
  :: Path b File
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.TestSuite
  -> IO (Maybe RuleInfo)
generateTestRule cabalFilePath pkgId dataFiles testsuite = do
  let testName = Text.pack $ Cabal.unUnqualComponentName $ Cabal.testName testsuite
      buildInfo = Cabal.testBuildInfo testsuite
      mainis = [ dropExtension path
               | Cabal.TestSuiteExeV10 _ path <- [Cabal.testInterface testsuite]
               ]
  generateRule
    cabalFilePath
    pkgId
    dataFiles
    buildInfo
    mainis
    TEST
    testName

generateBenchmarkRule
  :: Path b File
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.Benchmark
  -> IO (Maybe RuleInfo)
generateBenchmarkRule cabalFilePath pkgId dataFiles benchmark = do
  let benchName = Text.pack $ Cabal.unUnqualComponentName $ Cabal.benchmarkName benchmark
      buildInfo = Cabal.benchmarkBuildInfo benchmark
      mainis = [ dropExtension path
               | Cabal.BenchmarkExeV10 _ path <- [Cabal.benchmarkInterface benchmark]
               ]
  generateRule
    cabalFilePath
    pkgId
    dataFiles
    buildInfo
    mainis
    BENCH
    benchName

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
      deps =  depPackageNames bi
  hsSourceDirs <- mapM Path.parseRelDir (Cabal.hsSourceDirs bi)
  someModulePaths <- findModulePaths attrName cabalFilePath hsSourceDirs someModules
  otherModulePaths <- findModulePaths attrName cabalFilePath hsSourceDirs otherModules
  return $ Just $ RuleInfo
        { kind = componentTypeToRuleName ctype
        , name = attrName
        , cabalFile = pathToText cabalFilePath
        , importData = ImportData
          { deps
          , compilerFlags = versionMacro : optionsFromBuildInfo bi
          , extraLibraries = map Text.pack $ Cabal.extraLibs bi
          , tools = map toToolName $ Cabal.buildToolDepends bi
          }
        , attrs =
            [ ("version", TextValue pkgVersion)
            , ("srcs", StringListValue $ map pathToText $ someModulePaths ++ otherModulePaths)
            ] ++
            [ ("hidden_modules", StringListValue xs)
            | Just xs <- [hidden_modules]
            ] ++
            [ ("data", StringListValue $ map Text.pack dataFiles)
            | not (null dataFiles)
              -- The library always includes data files, and the other
              -- components must include them if they don't depend on the
              -- library.
            , ctype == LIB || pkgName `notElem` deps
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
  BENCH -> "haskell_binary"
  EXE -> "haskell_binary"
  LIB -> "haskell_library"
  TEST -> "haskell_test"

-- | Thrown when we can't find the file path of a Haskell
-- module which is referenced in a Cabal file.
data MissingModuleFile = MissingModuleFile
  { modulePath :: FilePath
  , cabalFile :: FilePath
  , componentName :: FilePath
  }
  deriving (Show, Exception)

-- | @findModulePaths componentName cabalFilePath hsSourceDirs someModules@
--
-- Finds out which files define the given modules under the directory where
-- the Cabal file is.
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
          raiseError = throwIO $ MissingModuleFile
            { modulePath = Path.toFilePath modulePath
            , cabalFile = Path.toFilePath cabalFilePath
            , componentName = Text.unpack componentName
            }
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
          extensions = [".hs", ".lhs", ".hsc"]
      findExtension extensions fullModulePath >>= \case
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

data UnresolvedCabalDependencies = UnresolvedCabalDependencies
  { cabalFile :: String
  , unresolvedDependencies :: [Cabal.Dependency]
  }
  deriving (Show, Exception)

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
    Left unresolvedDeps -> throwIO $ UnresolvedCabalDependencies
      { cabalFile
      , unresolvedDependencies = unresolvedDeps
      }
    Right (pd, _) -> return pd
