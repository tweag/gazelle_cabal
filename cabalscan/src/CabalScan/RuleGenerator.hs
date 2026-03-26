{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions to generate rules from Cabal files
module CabalScan.RuleGenerator
  ( generateRulesForCabalFile
  -- * Exported for tests
  , findModulePaths
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (filterM)
import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import qualified System.FilePath as Path
import qualified System.Directory as Path
import CabalScan.Rules
import CabalScan.DNF
import System.FilePath (dropExtension)
import Debug.Trace as Debug
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Flag
import Distribution.Types.CondTree as CondTree
import qualified Distribution.Types.BuildInfo.Lens as BL
import Distribution.Compat.Lens ((^.), (%~))
import Distribution.Types.ConfVar
import Distribution.Types.Condition
import Data.Char (toLower)

#if __GLASGOW_HASKELL__ == 810

import qualified Cabal.Cabal_8_10 as Cabal
import Cabal.Cabal_8_10 (ConfVar(PackageFlag))

#elif __GLASGOW_HASKELL__ == 900

import qualified Cabal.Cabal_9_0 as Cabal

#elif __GLASGOW_HASKELL__ == 902

import qualified Cabal.Cabal_9_2 as Cabal

#elif __GLASGOW_HASKELL__ >= 904

import qualified Cabal.Cabal_9_4 as Cabal

#else

#error GHC version __GLASGOW_HASKELL__ not supported

#endif

generateRulesForCabalFile :: FilePath -> IO PackageOutput
generateRulesForCabalFile cabalFilePath = do
  pd <- readCabalFile cabalFilePath
  let packageFlags = genPackageFlags pd
  let library = Cabal.condLibrary pd
      subLibraries = Cabal.condSubLibraries pd
      libraries = maybeToList library ++ (snd <$> subLibraries)
      executables = snd <$> Cabal.condExecutables pd
      testsuites = (\ (uqName, ts) -> mapTreeData (\ r -> r { Cabal.testName = uqName }) ts) <$> Cabal.condTestSuites pd
      benchmarks = (\ (uqName, ts) -> mapTreeData (\ r -> r { Cabal.benchmarkName = uqName }) ts) <$> Cabal.condBenchmarks pd
      pkgId = Cabal.package $ Cabal.packageDescription pd
      dataFiles = Cabal.dataFiles $ Cabal.packageDescription pd

  libRules <-
    traverse (generateRule LIB cabalFilePath pkgId dataFiles) libraries
  executablesRules <-
    traverse (generateRule EXE cabalFilePath pkgId dataFiles) executables
  testSuiteRules <-
    traverse (generateRule TEST cabalFilePath pkgId dataFiles) testsuites
  benchmarkRules <-
    traverse (generateRule BENCH cabalFilePath pkgId dataFiles) benchmarks
  let allRules = catMaybes $ libRules ++ executablesRules ++ testSuiteRules ++ benchmarkRules
      allConfigGroups = concatMap extractConfigGroups allRules
  return $ PackageOutput (mkFlag <$> packageFlags) allRules allConfigGroups
  where
    mkFlag flag = CabalScan.Rules.Flag (unFlagName $ flagName flag) (flagDefault flag)
    extractConfigGroups rule = ruleConfigGroups rule

dnf :: Condition ConfVar -> DNF

dnf (CAnd s1 s2) = dnfAnd (dnf s1) (dnf s2)

dnf (COr s1 s2)  = dnfOr (dnf s1) (dnf s2)

dnf (CNot cond) = negateDNF $ dnf cond

dnf (Lit b)       = dnfLit b
  where
    dnfLit :: Bool -> DNF
    dnfLit False = DnfFalse
    dnfLit True = DnfTrue

dnf (Var confVar)     = dnfVar confVar
  where
    dnfVar :: ConfVar -> DNF
    dnfVar (OS os) =
      let shortName = toLower <$> show os
          label = "@platforms//os:" <> shortName
      in mkVar label shortName
    dnfVar (Arch arch) =
      let shortName = show arch
          label = "@platforms//arch:" <> shortName
      in mkVar label shortName
    dnfVar (PackageFlag flag) =
      let shortName = unFlagName flag
          label = ":flag_" <> shortName
      in mkVar label shortName
    dnfVar (Impl Cabal.GHC range) =
      dnf $ Lit (Cabal.withinRange ghcVersion range)
    dnfVar (Impl flavor _) = error $ "unsupported compiler flavor " <> show flavor
#if __GLASGOW_HASKELL__ == 810
    -- just to please the exhaustiveness checker, which doesn't realize PackageFlag is a synonym for Flag
    dnfVar _ = error "never reached"
#endif

    mkVar :: String -> String -> DNF
    mkVar label shortName = Disj $ Set.singleton $ Conj $ Set.singleton $ JustT (VarT label shortName)

    ghcVersion = Cabal.mkVersion
      [ div __GLASGOW_HASKELL__ 100
      , mod __GLASGOW_HASKELL__ 10
      , __GLASGOW_HASKELL_PATCHLEVEL1__
      ]

data RuleDataCond = RuleDataCond {
  rdValue :: RuleData,
  rdBranches :: [(DNF, RuleData, Maybe RuleData)]
} deriving Show

data RuleDataBranch = RuleDataBranch {
  rdCond :: DNF,
  rdTrue :: RuleDataCond,
  rdFalse :: Maybe RuleDataCond
} deriving Show

class RuleConversion a where
  attrs :: Cabal.PackageIdentifier -> a -> Attributes
  getMainFile :: a -> Maybe FilePath
  ruleName :: Cabal.PackageIdentifier -> a -> String
  modules :: a -> [FilePath]
  hiddenMods :: a -> [String]

instance RuleConversion Cabal.Executable where
  attrs pkgId _ = pkgNamePrivAttr pkgId
  getMainFile = Just . Cabal.modulePath
  ruleName pkgId exe = let
      pkgName = pkgNameToString $ Cabal.pkgName pkgId
      exeName = Cabal.unUnqualComponentName $ Cabal.exeName exe
    in
      if exeName == pkgName then
        exeName <> "-binary"
      else
        exeName
  modules exe = filter (not . null) [Cabal.modulePath exe]
  hiddenMods _ = []

instance RuleConversion Cabal.Library where
  attrs = libPrivAttrs
  getMainFile _ = Nothing
  ruleName pkgId lib = case Cabal.libName lib of
    Cabal.LSubLibName name -> Cabal.unUnqualComponentName name
    _ -> pkgNameToString $ Cabal.pkgName pkgId
  modules lib = map Cabal.toFilePath $ Cabal.exposedModules lib
  hiddenMods lib =  [ qualifiedModulePath m | m <- Cabal.otherModules $ lib ^. BL.buildInfo ]
    where
      qualifiedModulePath = mconcat . intersperse "." . Cabal.components


instance RuleConversion Cabal.Benchmark where
  attrs pkgId _ = pkgNamePrivAttr pkgId
  getMainFile = listToMaybe . modules
  ruleName _ = Cabal.unUnqualComponentName . Cabal.benchmarkName
  modules benchmark = [path | Cabal.BenchmarkExeV10 _ path <- [Cabal.benchmarkInterface benchmark ] ]
  hiddenMods _ = []

instance RuleConversion Cabal.TestSuite where
  attrs pkgId _ = pkgNamePrivAttr pkgId
  getMainFile testsuite =
    let
      mainFiles = [ path | Cabal.TestSuiteExeV10 _ path <- [Cabal.testInterface testsuite] ]
    in
      listToMaybe mainFiles
  ruleName _ = Cabal.unUnqualComponentName . Cabal.testName
  modules testsuite = [ path | Cabal.TestSuiteExeV10 _ path <- [Cabal.testInterface testsuite] ]
  hiddenMods _ = []

getSources :: Cabal.BuildInfo -> FilePath -> String -> [FilePath] -> IO [FilePath]
getSources bi cabalFilePath attrName someModules = do
    let otherModules = map Cabal.toFilePath (Cabal.otherModules bi)
        hsSourceDirs = Cabal.hsSourceDirs bi
    someModulePaths <- findModulesPaths attrName cabalFilePath hsSourceDirs someModules
    otherModulePaths <- findModulesPaths attrName cabalFilePath hsSourceDirs otherModules
    return $ someModulePaths ++ otherModulePaths

generateRule
  :: forall v. (BL.HasBuildInfo v, RuleConversion v)
  => ComponentType
  -> FilePath
  -> Cabal.PackageIdentifier
  -> [FilePath]
  -> CondTree ConfVar [Cabal.Dependency] v
  -> IO (Maybe RuleInfo)
generateRule compType cabalFilePath pkgId dataFiles (CondNode var deps branches) =
  if var^.BL.buildable then do
    RuleDataCond { .. } <- handleNode var deps branches
    let pkgName = pkgNameToString $ Cabal.pkgName pkgId
        pkgVersion = Cabal.prettyShow $ Cabal.pkgVersion pkgId

        pkgDepNames = depPackageNames $ var^.BL.buildInfo
        versionMacro =
          "-DVERSION_" <> map underscorify pkgName <> "=" <> show pkgVersion
        hsSourceDirs = Cabal.hsSourceDirs $ var^.BL.buildInfo
        (selects, allConfigGroups) = unzip $ map dnfToSelect rdBranches
        configGroups = concat allConfigGroups

    mainFile <- case getMainFile var of
      Just path -> do
        ret <- findMainFile cabalFilePath path hsSourceDirs
        return $ Just ret
      Nothing -> return Nothing

    return $ Just $ RuleInfo {
      name = ruleName pkgId var,
      kind = componentTypeToRuleName compType,
      cabalFile = cabalFilePath,
      version = pkgVersion,
      srcs = get rSrcs rdValue selects,
      hiddenModules = nonEmpty $ get rHiddenModules rdValue selects,
      mainFile = mainFile,
      dataAttr =
            -- The library always includes data files, and the other
            -- components must include them if they don't depend on the
            -- library.
            if compType == LIB || pkgName `notElem` pkgDepNames
            then nonEmpty dataFiles
            else Nothing,
      privateAttrs = attrs pkgId var,
      ruleConfigGroups = configGroups,
      importData = ImportData {
         deps = get rDeps rdValue selects
         , ghcOpts = get rGhcOpts (rdValue { rGhcOpts = versionMacro : rGhcOpts rdValue}) selects
         , extraLibraries = get rExtraLibraries rdValue selects
         , tools = get rTools rdValue selects
                              }
      }
  else
    return Nothing
 where
    toToolName (Cabal.ExeDependency pkg exe _) =
      ToolName (pkgNameToString pkg) (Cabal.unUnqualComponentName exe)

    convert :: v -> IO RuleData
    convert compVar = do
      let mods = map dropExtension $ modules compVar
          bi = compVar ^. BL.buildInfo
          attrName = ruleName pkgId compVar
          componentDepNames = depPackageNames bi

      srcs <- getSources bi cabalFilePath attrName mods
      return $ RuleData
        {
        rDeps = componentDepNames
        , rGhcOpts = optionsFromBuildInfo bi
        , rExtraLibraries = Cabal.extraLibs bi
        , rTools = map toToolName $ Cabal.buildToolDepends bi
        , rSrcs = srcs
        , rHiddenModules = hiddenMods compVar
        }

    get :: (Eq a, Monoid a) => (RuleData -> a) -> RuleData -> [Configurable RuleData] -> [Configurable a]
    get getter rdValue selects = [
      s | select <- Value (getter rdValue) : ((getter <$>) <$> selects),
        let s = simplifySelect select,
        s /= Value mempty ]

    simplifySelect :: Eq a => Configurable a -> Configurable a
    -- if all branches map to the same value, we can simplify
    simplifySelect select@(Select m (Just e))
      | all (e ==) (Map.elems m) = Value e
      | otherwise = select
    simplifySelect a = a

    dnfToSelect :: (DNF, RuleData, Maybe RuleData) -> (Configurable RuleData, [ConfigSettingGroup])
    dnfToSelect (DnfFalse, _, falseVal) = (Value $ fromMaybe mempty falseVal, [])
    dnfToSelect (DnfTrue, trueVal, _) = (Value trueVal, [])
    dnfToSelect (Disj cs, trueVal, falseVal)
      -- Case 1: True and false branches are equal
      | Just trueVal == falseVal = (Value trueVal, [])

      -- Case 2: Simple OR of single variables (a || b || c)
      | all isSinglePosVar cs
      = let vars = Set.fromList $ mapMaybe extractSinglePosVar $ Set.toList cs
            defaultVal = fromMaybe mempty falseVal
        in (Select (Map.fromSet (const trueVal) vars) (Just defaultVal), [])

      -- Case 3: All conjunctions share a common negated variable (!x && a) || (!x && b)
      -- This can be rewritten as: if x then default else (a || b)
      | Just negVar <- findCommonNegation cs
      = let otherTerms = Set.unions $ Set.map (deleteCommonNeg negVar) cs
        in if all (not . isNegTerm) otherTerms
           then let defaultMap = maybe Map.empty (Map.singleton negVar) falseVal
                    otherVars = Set.map termVar otherTerms
                in (Select (defaultMap `Map.union` Map.fromSet (const trueVal) otherVars)
                          (Just $ fromMaybe mempty falseVal), [])
           else unsupported

      -- Case 4: Conjunctions with only positive variables (a && b) || (c && d)
      -- Use config_setting_group for AND chaining
      | all onlyPosVars cs
      = let (selectMap, groups) = buildConfigGroups cs trueVal falseVal
        in (Select selectMap (Just $ fromMaybe mempty falseVal), groups)

      -- Case 5: Complex condition not yet implemented
      | otherwise = unsupported
      where
        unsupported = (traceShow cs $ Select (Map.singleton "not-implemented" trueVal) falseVal, [])

        isSinglePosVar (Conj ts)
          | Set.size ts == 1
          , SinglePosVarTerm _ _ <- Set.elemAt 0 ts = True
          | otherwise = False

        onlyPosVars (Conj ts) = all (not . isNegTerm) $ Set.toList ts

        -- Extract variable name from a single positive variable conjunction
        extractSinglePosVar (SinglePosVar v) = Just v
        extractSinglePosVar _ = Nothing

        -- Build config_setting_groups for AND-ed conditions
        buildConfigGroups conjs trueV _falseV =
          let mkGroup (Conj ts) =
                let terms = Set.toList ts
                    vars = map termVar terms
                    shortNames = map termShortName terms
                    -- Create name from short names joined with "_and_"
                    groupName = ":" ++ intercalate "_and_" shortNames
                in (groupName, ConfigSettingGroup groupName vars)
              groupsWithNames = map mkGroup $ Set.toList conjs
              selectMap = Map.fromList [(name, trueV) | (name, _) <- groupsWithNames]
              groups = map snd groupsWithNames
          in (selectMap, groups)

        -- Find a negated variable that appears in all conjunctions
        findCommonNegation conjs
          | Set.size commonTerms == 1
          , NotT (VarT lbl _) <- Set.elemAt 0 commonTerms
          , all ((== 2) . conjSize) conjs
          = Just lbl
          | otherwise = Nothing
          where
            commonTerms = foldl1 Set.intersection (Set.map getConjTerms conjs)
            conjSize (Conj ts) = Set.size ts

        getConjTerms (Conj ts) = ts

        deleteCommonNeg v (Conj ts) = Set.filter (\t -> termVar t /= v) ts

    handleNode :: v -> [Cabal.Dependency] -> [CondBranch ConfVar [Cabal.Dependency] v] -> IO RuleDataCond
    handleNode compVar _dependencies branchList = do
      rd <- convert compVar
      bs <- traverse (handleBranch compVar) branchList

      return $ RuleDataCond rd (foldr combineBranches [] bs)

    combineBranches :: RuleDataBranch -> [(DNF, RuleData, Maybe RuleData)] -> [(DNF, RuleData, Maybe RuleData)]
    combineBranches RuleDataBranch{..} acc =
      let trueBranches = map (\(cond, val, mval) -> (dnfAnd rdCond cond, val, mval)) (rdBranches rdTrue)
          falseBranches = case rdFalse of
            Nothing -> []
            Just falseCond -> map (\(cond, val, mval) -> (dnfAnd (negateDNF rdCond) cond, val, mval)) (rdBranches falseCond)
          currentBranch = (rdCond, rdValue rdTrue, rdValue <$> rdFalse)
      in currentBranch : trueBranches ++ falseBranches ++ acc

    addSourceDirs :: v -> v -> v
    addSourceDirs v = BL.hsSourceDirs %~ (<> (v ^. BL.hsSourceDirs))

    goNext v (CondNode compVar depList branchList) =
      handleNode (addSourceDirs v compVar) depList branchList

    -- CondBranch condition (CondTree x) (Maybe  CondTree y)
    handleBranch :: v -> CondBranch ConfVar [Cabal.Dependency] v -> IO RuleDataBranch
    handleBranch compVar (CondBranch cond ifTrue maybeIfFalse) = do
      let condAsDNF = dnf cond
      trueBranch <- goNext compVar ifTrue
      falseBranch <- traverse (goNext compVar) maybeIfFalse

      return $ RuleDataBranch condAsDNF trueBranch falseBranch

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
findModulesPaths componentName cabalFilePath hsSourceDirs moduleNames =
  concat <$> traverse findModules moduleNames
  where
    cabalDir = Path.takeDirectory cabalFilePath
    findModules :: FilePath -> IO [FilePath]
    findModules modulePath = do
      let raiseError = throwIO $ traceStack "missing module file" $ MissingModuleFile
            { modulePath = modulePath
            , cabalFile = cabalFilePath
            , componentName = componentName
            }
      findModulePaths cabalDir hsSourceDirs modulePath >>= \case
        [] -> raiseError
        foundModulePaths -> pure foundModulePaths


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

depPackageNames :: Cabal.BuildInfo -> [String]
depPackageNames = concatMap depNames . Cabal.targetBuildDepends

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

readCabalFile :: FilePath -> IO Cabal.GenericPackageDescription
readCabalFile cabalFilePath = do
  let cabalFile = cabalFilePath
  Cabal.readGenericPackageDescription Cabal.silent cabalFile
