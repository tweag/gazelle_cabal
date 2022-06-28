{-#LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ == 810

module Cabal.Cabal_8_10(module Cabal)

where

import Distribution.Compiler as Cabal (CompilerFlavor(GHC), perCompilerFlavorToList, unknownCompilerInfo, CompilerId(CompilerId), AbiTag(NoAbiTag))
import Distribution.ModuleName as Cabal (toFilePath, components)
import Distribution.Package as Cabal ()
import Distribution.PackageDescription as Cabal (allLibraries, executables, testSuites, benchmarks, package, dataFiles, Library, libName, exposedModules, libBuildInfo, LibraryName(LSubLibName), Executable, exeName, buildInfo, hsSourceDirs, modulePath, TestSuite, testName, testBuildInfo, TestSuiteInterface(TestSuiteExeV10), testInterface, Benchmark, benchmarkName, benchmarkBuildInfo, BenchmarkInterface(BenchmarkExeV10), benchmarkInterface, BuildInfo, buildable, otherModules, extraLibs)
import Distribution.PackageDescription.Configuration as Cabal (finalizePD)
import Distribution.PackageDescription.Parsec as Cabal (readGenericPackageDescription)
import Distribution.Types.BuildInfo as Cabal (buildToolDepends, targetBuildDepends, defaultExtensions, cppOptions, ldOptions, options)
import Distribution.Types.ComponentRequestedSpec as Cabal (ComponentRequestedSpec(ComponentRequestedSpec), testsRequested, benchmarksRequested)
import Distribution.Types.Dependency as Cabal (Dependency, depLibraries, depPkgName)
import Distribution.Types.ExeDependency as Cabal (ExeDependency(ExeDependency))
import Distribution.Types.Library as Cabal (libVisibility)
import Distribution.Types.LibraryVisibility as Cabal (LibraryVisibility(LibraryVisibilityPrivate))
import Distribution.Types.PackageDescription as Cabal (PackageDescription)
import Distribution.Types.PackageId as Cabal (PackageIdentifier, pkgName, pkgVersion)
import Distribution.Types.PackageName as Cabal (PackageName, unPackageName)
import Distribution.Types.UnqualComponentName as Cabal (unUnqualComponentName)
import Distribution.Types.Version as Cabal (mkVersion)
import Distribution.Pretty as Cabal (prettyShow)
import Distribution.System as Cabal (Platform(Platform), buildArch, buildOS)
import Distribution.Verbosity as Cabal (normal)

#else

module Cabal.Cabal_8_10 where

#endif
