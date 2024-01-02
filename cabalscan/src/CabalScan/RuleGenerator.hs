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
  , findModulePaths
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (filterM)
import Data.List (intersperse)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set.Internal as Set (toList)
import qualified System.FilePath as Path
import qualified System.Directory as Path
import CabalScan.Rules
import System.FilePath (dropExtension)

#if __GLASGOW_HASKELL__ == 810

import qualified Cabal.Cabal_8_10 as Cabal

#elif __GLASGOW_HASKELL__ == 900

import qualified Cabal.Cabal_9_0 as Cabal

#elif __GLASGOW_HASKELL__ == 902

import qualified Cabal.Cabal_9_2 as Cabal

#elif __GLASGOW_HASKELL__ == 904

import qualified Cabal.Cabal_9_4 as Cabal

#else

import qualified Cabal.Cabal_9_6 as Cabal

#endif

generateRulesForCabalFile :: FilePath -> IO [RuleInfo]
generateRulesForCabalFile cabalFilePath = do
  pd <- readCabalFile cabalFilePath
  let libraries = Cabal.allLibraries pd
      executables = Cabal.executables pd
      testsuites = Cabal.testSuites pd
      benchmarks = Cabal.benchmarks pd
      pkgId = Cabal.package pd
      dataFiles = Cabal.dataFiles pd
  libraryRules <-
    traverse (generateLibraryRule cabalFilePath pkgId dataFiles) libraries
  executablesRules <-
    traverse (generateBinaryRule cabalFilePath pkgId dataFiles) executables
  testSuiteRules <-
    traverse (generateTestRule cabalFilePath pkgId dataFiles) testsuites
  benchmarkRules <-
    traverse (generateBenchmarkRule cabalFilePath pkgId dataFiles) benchmarks
  return $ catMaybes $ libraryRules ++ executablesRules ++ testSuiteRules ++ benchmarkRules

generateLibraryRule
  :: FilePath
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.Library
  -> IO (Maybe RuleInfo)
generateLibraryRule cabalFilePath pkgId dataFiles lib = do
  let libraryName = obtainLibraryName $ Cabal.libName lib
      exposedModules = map Cabal.toFilePath $ Cabal.exposedModules lib
      buildInfo = Cabal.libBuildInfo lib
      privAttrs = libPrivAttrs pkgId lib
  generateRule
    cabalFilePath
    pkgId
    dataFiles
    buildInfo
    exposedModules
    LIB
    libraryName
    Nothing
    privAttrs
  where
    obtainLibraryName :: Cabal.LibraryName -> String
    obtainLibraryName (Cabal.LSubLibName name) = Cabal.unUnqualComponentName $ name
    obtainLibraryName _ = pkgNameToString $ Cabal.pkgName pkgId

generateBinaryRule
  :: FilePath
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.Executable
  -> IO (Maybe RuleInfo)
generateBinaryRule cabalFilePath pkgId dataFiles executable = do
  let pkgName = pkgNameToString $ Cabal.pkgName pkgId
      exeName = Cabal.unUnqualComponentName $ Cabal.exeName executable
      targetName =
        if exeName == pkgName then
          exeName <> "-binary"
        else
          exeName
      buildInfo = Cabal.buildInfo executable
      srcDirs = Cabal.hsSourceDirs buildInfo
      mainFilePath = Cabal.modulePath executable
      modules = [dropExtension mainFilePath]
      privAttrs = pkgNamePrivAttr pkgId
  mainFile <- findMainFile cabalFilePath mainFilePath srcDirs
  generateRule
    cabalFilePath
    pkgId
    dataFiles
    buildInfo
    modules
    EXE
    targetName
    (Just mainFile)
    privAttrs

generateTestRule
  :: FilePath
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.TestSuite
  -> IO (Maybe RuleInfo)
generateTestRule cabalFilePath pkgId dataFiles testsuite = do
  let testName = Cabal.unUnqualComponentName $ Cabal.testName testsuite
      buildInfo = Cabal.testBuildInfo testsuite
      srcDirs = Cabal.hsSourceDirs buildInfo
      mainFiles = [ path | Cabal.TestSuiteExeV10 _ path <- [Cabal.testInterface testsuite] ]
      mainModules = map dropExtension mainFiles
      privAttrs = pkgNamePrivAttr pkgId
  mainRelPaths <- sequence [ findMainFile cabalFilePath mainis srcDirs | mainis <- mainFiles ]
  generateRule
    cabalFilePath
    pkgId
    dataFiles
    buildInfo
    mainModules
    TEST
    testName
    (listToMaybe mainRelPaths)
    privAttrs

generateBenchmarkRule
  :: FilePath
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.Benchmark
  -> IO (Maybe RuleInfo)
generateBenchmarkRule cabalFilePath pkgId dataFiles benchmark = do
  let benchName = Cabal.unUnqualComponentName $ Cabal.benchmarkName benchmark
      buildInfo = Cabal.benchmarkBuildInfo benchmark
      srcDirs = Cabal.hsSourceDirs buildInfo
      mainFiles = [path | Cabal.BenchmarkExeV10 _ path <- [Cabal.benchmarkInterface benchmark]]
      mainModule = map dropExtension mainFiles
      privAttrs = pkgNamePrivAttr pkgId
  mainRelPaths <- sequence [ findMainFile cabalFilePath mainis srcDirs | mainis <- mainFiles ]
  generateRule
    cabalFilePath
    pkgId
    dataFiles
    buildInfo
    mainModule
    BENCH
    benchName
    (listToMaybe mainRelPaths)
    privAttrs

generateRule
  :: FilePath
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.BuildInfo
  -> [FilePath]
  -> ComponentType
  -> String
  -> Maybe FilePath
  -> Attributes
  -> IO (Maybe RuleInfo)
generateRule _ _ _ bi _ _ _ _ _ | not (Cabal.buildable bi) = return Nothing
generateRule cabalFilePath pkgId dataFiles bi someModules ctype attrName mainFile privAttrs = do
  let pkgName = pkgNameToString $ Cabal.pkgName pkgId
      pkgVersion = Cabal.prettyShow $ Cabal.pkgVersion pkgId
      versionMacro =
        "-DVERSION_" <> map underscorify pkgName <> "=" <> show pkgVersion
      otherModules = map Cabal.toFilePath (Cabal.otherModules bi)
      deps =  depPackageNames bi
  let hsSourceDirs = Cabal.hsSourceDirs bi
  someModulePaths <- findModulesPaths attrName cabalFilePath hsSourceDirs someModules
  otherModulePaths <- findModulesPaths attrName cabalFilePath hsSourceDirs otherModules
  return $ Just $ RuleInfo
        { kind = componentTypeToRuleName ctype
        , name = attrName
        , cabalFile = cabalFilePath
        , importData = ImportData
          { deps
          , ghcOpts = versionMacro : optionsFromBuildInfo bi
          , extraLibraries = Cabal.extraLibs bi
          , tools = map toToolName $ Cabal.buildToolDepends bi
          }
        , version = pkgVersion
        , srcs = someModulePaths ++ otherModulePaths
        , hiddenModules
        , dataAttr =
            -- The library always includes data files, and the other
            -- components must include them if they don't depend on the
            -- library.
            if ctype == LIB || pkgName `notElem` deps
            then nonEmpty dataFiles
            else Nothing
        , mainFile = mainFile
        , privateAttrs = privAttrs
        }
  where
    hiddenModules = case ctype of
      LIB -> nonEmpty [ qualifiedModulePath m | m <- Cabal.otherModules bi ]
      _ -> Nothing

    qualifiedModulePath = mconcat . intersperse "." . Cabal.components

    toToolName (Cabal.ExeDependency pkg exe _) =
      ToolName (pkgNameToString pkg) (Cabal.unUnqualComponentName exe)

    underscorify '-' = '_'
    underscorify c = c

-- | Thrown when we can't find the file path of a main
-- file which is referenced in a Cabal file (hs-source-dirs + main-is).
data MainFileNotFound = MainFileNotFound
  { cabalFile :: FilePath
  , mainFile :: FilePath
  , hsSourceDirs :: [FilePath]
  }
  deriving (Show, Exception)

-- | Thrown when we find multiple main files
-- under hs-source-dirs
data MultipleMainFilesFound = MultipleMainFilesFound
  { cabalFile :: FilePath
  , mainFile :: FilePath
  , hsSourceDirs :: [FilePath]
  , foundFiles :: [FilePath]
  }
  deriving (Show, Exception)

-- | @findMainFile cabalFile mainFile hsSrcDirs@
--
-- Finds out relative path to the main file under the directory where
-- the Cabal file is. It does simple concatenation between @hsSrcDir@ and
-- the @mainFile@ and validates the existence of such path.
--
findMainFile :: FilePath -> FilePath -> [FilePath] -> IO FilePath
findMainFile cabalFile mainPath hsSrcDirs = do
  let parentDir = Path.takeDirectory cabalFile
  let mainPaths = [ dir Path.</> mainPath | dir <- hsSrcDirs ]
  validPaths <- filterM (Path.doesPathExist . (Path.</>) parentDir) mainPaths
  case validPaths of
    [path] -> return path
    []   -> throwIO MainFileNotFound
             { cabalFile = cabalFile
             , mainFile = mainPath
             , hsSourceDirs = hsSrcDirs
             }
    paths     -> throwIO MultipleMainFilesFound
             { cabalFile = cabalFile
             , mainFile = mainPath
             , hsSourceDirs = hsSrcDirs
             , foundFiles = paths
             }

pkgNamePrivAttr :: Cabal.PackageIdentifier -> Attributes
pkgNamePrivAttr pkgId = [ ("pkgName", packageName) ]
  where packageName = StringValue . pkgNameToString $ Cabal.pkgName pkgId

libPrivAttrs :: Cabal.PackageIdentifier -> Cabal.Library -> Attributes
libPrivAttrs pkgId lib = pkgNameAttr ++ visibilityAttr
  where
    pkgNameAttr = pkgNamePrivAttr pkgId
    visibilityAttr = [ ("visibility", obtainVisibilityAttr) ]
    obtainVisibilityAttr = StringValue $ case Cabal.libVisibility lib of
                                             Cabal.LibraryVisibilityPrivate -> "private"
                                             _                              -> "public"

componentTypeToRuleName :: ComponentType -> String
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

-- | @findModulesPaths componentName cabalFilePath hsSourceDirs someModules@
--
-- Finds out which files define the given modules under the directory where
-- the Cabal file is.
--
-- @componentName@ is used for error reporting only.
--
findModulesPaths
  :: String -> FilePath -> [FilePath] -> [FilePath] -> IO [FilePath]
findModulesPaths componentName cabalFilePath hsSourceDirs moduleNames = do
  concat <$> traverse findModules moduleNames
  where
    cabalDir = Path.takeDirectory cabalFilePath
    findModules :: FilePath -> IO [FilePath]
    findModules modulePath = do
      let raiseError = throwIO $ MissingModuleFile
            { modulePath = modulePath
            , cabalFile = cabalFilePath
            , componentName = componentName
            }
      findModulePaths cabalDir hsSourceDirs modulePath >>= \case
        [] -> raiseError
        foundModulePaths -> pure foundModulePaths

depPackageNames :: Cabal.BuildInfo -> [String]
depPackageNames = concatMap depNames . Cabal.targetBuildDepends
    where
      depNames :: Cabal.Dependency -> [String]
      depNames dep =
        let
          pkgName :: String
          pkgName = pkgNameToString $ Cabal.depPkgName dep
          identifierOf :: Cabal.LibraryName -> String
          identifierOf (Cabal.LSubLibName name) = pkgName <> ":" <> Cabal.unUnqualComponentName name
          identifierOf _ = pkgName
        in
          map identifierOf $ Set.toList $ Cabal.depLibraries dep

-- | @findModulePaths parentDir hsSourceDirs modulePath@ finds
-- the paths of the module, relative to @hsSourceDirs@.
--
-- The input module path must be relative to some of the directories in
-- @hsSourceDirs@ and must not include an extension. The output of
-- this function will include the actual extension and is relative
-- to @parentDir@.
--
-- The directories in @hsSourceDirs@ must be relative to @parentDir@.
findModulePaths :: FilePath -> [FilePath] -> FilePath -> IO [FilePath]
findModulePaths parentDir hsSourceDirs modulePath =
  case hsSourceDirs of
    [] -> return []
    srcDir:otherDirs -> do
      let fullModulePath = parentDir Path.</> srcDir Path.</> modulePath
          extensions = [".hs", ".lhs", ".hsc", ".hs-boot", ".lhs-boot"]

      findExtensions extensions fullModulePath >>= \case
        [] -> findModulePaths parentDir otherDirs modulePath
        foundExtensions -> return $ map (Path.addExtension (srcDir Path.</> modulePath)) foundExtensions
  where
    findExtensions :: [String] -> FilePath -> IO [String]
    findExtensions exts filepath =
      filterM (Path.doesPathExist . Path.addExtension filepath) exts

pkgNameToString :: Cabal.PackageName -> String
pkgNameToString = Cabal.unPackageName

-- | Extracts ghc-options and language extensions and returns
-- them as flags for ghc.
optionsFromBuildInfo :: Cabal.BuildInfo -> [String]
optionsFromBuildInfo bi =
  map (("-X" <>) . Cabal.prettyShow) (Cabal.defaultExtensions bi)
  ++ ghcOptions
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

readCabalFile :: FilePath -> IO Cabal.PackageDescription
readCabalFile cabalFilePath = do
  let cabalFile = cabalFilePath
  genericPkg <- Cabal.readGenericPackageDescription Cabal.silent cabalFile
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
