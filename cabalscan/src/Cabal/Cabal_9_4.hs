{-#LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ == 904

module Cabal.Cabal_9_4(module Cabal, depLibraries, hsSourceDirs)

where

import Distribution.Compiler as Cabal (CompilerFlavor(GHC), perCompilerFlavorToList, unknownCompilerInfo, CompilerId(CompilerId), AbiTag(NoAbiTag))
import Distribution.ModuleName as Cabal (toFilePath, components)
import Distribution.Package as Cabal ()
import Distribution.PackageDescription as Cabal (allLibraries, executables, testSuites, benchmarks, package, dataFiles, Library, libName, exposedModules, libBuildInfo, Executable, exeName, buildInfo, modulePath, TestSuite, testName, testBuildInfo, TestSuiteInterface(TestSuiteExeV10), testInterface, Benchmark, benchmarkName, benchmarkBuildInfo, BenchmarkInterface(BenchmarkExeV10), benchmarkInterface, BuildInfo, buildable, otherModules, extraLibs)
import Distribution.PackageDescription.Configuration as Cabal (finalizePD)
import Distribution.Simple.PackageDescription as Cabal (readGenericPackageDescription)
import Distribution.Types.BuildInfo as Cabal (buildToolDepends, targetBuildDepends, defaultExtensions, cppOptions, ldOptions, options)
import Distribution.Types.ComponentRequestedSpec as Cabal (ComponentRequestedSpec(ComponentRequestedSpec), testsRequested, benchmarksRequested)
import Distribution.Types.Dependency as Cabal (Dependency, depPkgName)
import Distribution.Types.ExeDependency as Cabal (ExeDependency(ExeDependency))
import Distribution.Types.Library as Cabal (libVisibility)
import Distribution.Types.LibraryName as Cabal (LibraryName(LSubLibName))
import Distribution.Types.LibraryVisibility as Cabal (LibraryVisibility(LibraryVisibilityPrivate))
import Distribution.Types.PackageDescription as Cabal (PackageDescription)
import Distribution.Types.PackageId as Cabal (PackageIdentifier, pkgName, pkgVersion)
import Distribution.Types.PackageName as Cabal (PackageName, unPackageName)
import Distribution.Types.UnqualComponentName as Cabal (unUnqualComponentName)
import Distribution.Types.Version as Cabal (mkVersion)
import Distribution.Pretty as Cabal (prettyShow)
import Distribution.System as Cabal (Platform(Platform), buildArch, buildOS)
import Distribution.Verbosity as Cabal (silent)

import qualified Distribution.Types.Dependency as C (depLibraries)
import qualified Distribution.Compat.NonEmptySet as C (toSet)
import Data.Set.Internal (Set)
import qualified Distribution.PackageDescription as C (hsSourceDirs)
import qualified Distribution.Utils.Path as C (getSymbolicPath)

depLibraries :: Cabal.Dependency -> Set Cabal.LibraryName
depLibraries = C.toSet . C.depLibraries

hsSourceDirs :: Cabal.BuildInfo -> [FilePath]
hsSourceDirs = map C.getSymbolicPath . C.hsSourceDirs

#else

module Cabal.Cabal_9_4 where

#endif
