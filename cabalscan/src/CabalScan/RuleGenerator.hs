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
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Set.Internal as Set (toList)
import qualified Data.Text as Text
import qualified Distribution.Compiler as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.ComponentRequestedSpec as Cabal
import qualified Distribution.Types.ExeDependency as Cabal
import qualified Distribution.Types.LibraryVisibility as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.System as Cabal
import qualified Distribution.Verbosity as Verbosity
import Path (Path, Rel, Dir, File)
import qualified Path as Path
import qualified Path.IO as Path
import CabalScan.Rules
import System.FilePath (dropExtension)

generateRulesForCabalFile :: Path b File -> IO [RuleInfo]
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
  :: Path b File
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
    obtainLibraryName :: Cabal.LibraryName -> Text
    obtainLibraryName (Cabal.LSubLibName name) = Text.pack . Cabal.unUnqualComponentName $ name
    obtainLibraryName _ = pkgNameToText $ Cabal.pkgName pkgId

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
  :: Path b File
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.TestSuite
  -> IO (Maybe RuleInfo)
generateTestRule cabalFilePath pkgId dataFiles testsuite = do
  let testName = Text.pack $ Cabal.unUnqualComponentName $ Cabal.testName testsuite
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
  :: Path b File
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.Benchmark
  -> IO (Maybe RuleInfo)
generateBenchmarkRule cabalFilePath pkgId dataFiles benchmark = do
  let benchName = Text.pack $ Cabal.unUnqualComponentName $ Cabal.benchmarkName benchmark
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
  :: Path b File
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> Cabal.BuildInfo
  -> [FilePath]
  -> ComponentType
  -> Text
  -> Maybe FilePath
  -> Attributes
  -> IO (Maybe RuleInfo)
generateRule _ _ _ bi _ _ _ _ _ | not (Cabal.buildable bi) = return Nothing
generateRule cabalFilePath pkgId dataFiles bi someModules ctype attrName mainFile privAttrs = do
  let pkgName = pkgNameToText $ Cabal.pkgName pkgId
      pkgVersion = Text.pack $ Cabal.prettyShow $ Cabal.pkgVersion pkgId
      versionMacro =
        "-DVERSION_" <> Text.replace "-" "_" pkgName <> "=" <> Text.pack (show pkgVersion)
      otherModules = map Cabal.toFilePath (Cabal.otherModules bi)
      deps =  depPackageNames bi
  hsSourceDirs <- mapM Path.parseRelDir (Cabal.hsSourceDirs bi)
  someModulePaths <- findModulesPaths attrName cabalFilePath hsSourceDirs someModules
  otherModulePaths <- findModulesPaths attrName cabalFilePath hsSourceDirs otherModules
  return $ Just $ RuleInfo
        { kind = componentTypeToRuleName ctype
        , name = attrName
        , cabalFile = pathToText cabalFilePath
        , importData = ImportData
          { deps
          , ghcOpts = versionMacro : optionsFromBuildInfo bi
          , extraLibraries = map Text.pack $ Cabal.extraLibs bi
          , tools = map toToolName $ Cabal.buildToolDepends bi
          }
          , version = TextValue pkgVersion
          , srcs = StringListValue $ map pathToText $ someModulePaths ++ otherModulePaths
          , hidden_modules =
             case hidden_modules of
               Just xs@(_:_) -> Just $ StringListValue xs
               _ -> Nothing
          , dataAttr =
              -- The library always includes data files, and the other
              -- components must include them if they don't depend on the
              -- library.
              if not (null dataFiles) && (ctype == LIB || pkgName `notElem` deps)
              then Just $ StringListValue $ map Text.pack dataFiles
              else Nothing
          , main_file =
              fmap (TextValue . Text.pack) mainFile
         , privateAttrs = privAttrs
        }
  where
    pathToText = Text.pack . Path.toFilePath

    hidden_modules = case ctype of
      LIB -> Just [ qualifiedModulePath m | m <- Cabal.otherModules bi ]
      _ -> Nothing

    qualifiedModulePath = mconcat . intersperse "." . map Text.pack . Cabal.components

    toToolName (Cabal.ExeDependency pkg exe _) =
      ToolName (pkgNameToText pkg) (Text.pack $ Cabal.unUnqualComponentName exe)

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
findMainFile :: Path b File -> FilePath -> [FilePath] -> IO FilePath
findMainFile cabalFile mainFile hsSrcDirs = do
  let parentDir = Path.parent cabalFile
  mainPath <- Path.parseRelFile mainFile
  srcDirs <- mapM Path.parseRelDir hsSrcDirs
  let mainPaths = [ dir Path.</> mainPath | dir <- srcDirs ]
  validPaths <- filterM (Path.doesFileExist . ((Path.</>) parentDir)) mainPaths
  case validPaths of
    [path] -> return $ Path.toFilePath path
    []   -> throwIO MainFileNotFound
             { cabalFile = Path.toFilePath cabalFile
             , mainFile = mainFile
             , hsSourceDirs = hsSrcDirs
             }
    paths     -> throwIO MultipleMainFilesFound
             { cabalFile = Path.toFilePath cabalFile
             , mainFile = mainFile
             , hsSourceDirs = hsSrcDirs
             , foundFiles = map Path.toFilePath paths
             }

pkgNamePrivAttr :: Cabal.PackageIdentifier -> Attributes
pkgNamePrivAttr pkgId = [ ("pkgName", packageName) ]
  where packageName = TextValue . pkgNameToText $ Cabal.pkgName pkgId

libPrivAttrs :: Cabal.PackageIdentifier -> Cabal.Library -> Attributes
libPrivAttrs pkgId lib = pkgNameAttr ++ visibilityAttr
  where
    pkgNameAttr = pkgNamePrivAttr pkgId
    visibilityAttr = [ ("visibility", obtainVisibilityAttr) ]
    obtainVisibilityAttr = TextValue $ case Cabal.libVisibility lib of
                                             Cabal.LibraryVisibilityPrivate -> "private"
                                             _                              -> "public"

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

-- | @findModulesPaths componentName cabalFilePath hsSourceDirs someModules@
--
-- Finds out which files define the given modules under the directory where
-- the Cabal file is.
--
-- @componentName@ is used for error reporting only.
--
findModulesPaths
  :: Text -> Path b File -> [Path Rel Dir] -> [FilePath] -> IO [Path Rel File]
findModulesPaths componentName cabalFilePath hsSourceDirs moduleNames = do
  modulesAsPaths <- traverse Path.parseRelFile moduleNames
  concat <$> traverse findModules modulesAsPaths
  where
    cabalDir = Path.parent cabalFilePath
    findModules :: Path Rel File -> IO [Path Rel File]
    findModules modulePath = do
      let raiseError = throwIO $ MissingModuleFile
            { modulePath = Path.toFilePath modulePath
            , cabalFile = Path.toFilePath cabalFilePath
            , componentName = Text.unpack componentName
            }
      findModulePaths cabalDir hsSourceDirs modulePath >>= \case
        [] -> raiseError
        foundModulePaths -> pure foundModulePaths

depPackageNames :: Cabal.BuildInfo -> [Text]
depPackageNames = concatMap depNames . Cabal.targetBuildDepends
    where
      depNames :: Cabal.Dependency -> [Text]
      depNames dep =
        let
          pkgName :: Text
          pkgName = pkgNameToText $ Cabal.depPkgName dep
          identifierOf :: Cabal.LibraryName -> Text
          identifierOf (Cabal.LSubLibName name) = pkgName <> ":" <> Text.pack (Cabal.unUnqualComponentName name)
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
findModulePaths :: Path b Dir -> [Path Rel Dir] -> Path Rel File -> IO [Path Rel File]
findModulePaths parentDir hsSourceDirs modulePath =
  case hsSourceDirs of
    [] -> return []
    srcDir:otherDirs -> do
      let fullModulePath = parentDir Path.</> srcDir Path.</> modulePath
          extensions = [".hs", ".lhs", ".hsc", ".hs-boot", ".lhs-boot"]

      findExtensions extensions fullModulePath >>= \case
        [] -> findModulePaths parentDir otherDirs modulePath
        foundExtensions -> traverse (\ext -> Path.addExtension ext (srcDir Path.</> modulePath)) foundExtensions
  where
    findExtensions :: [String] -> Path absrel File -> IO [String]
    findExtensions exts filepath =
      filterM (\ext -> Path.addExtension ext filepath >>= Path.doesFileExist) exts

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
