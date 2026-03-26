{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CabalScan.Rules where

import qualified Text.JSON as Json
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function ((&))
-- | Information about rules to give to bazel
--
-- > RuleInfo
-- >   { kind = "haskell_library"
-- >   , name = "foo"
-- >   , importData = ImportData
-- >     { deps = ["protolude"]
-- >     , extraLibraries = ["libsodium"]
-- >     , ghcOpts = ["-Werror", "-Wall"]
-- >     , tools = [ToolName "tasty-discover" "tasty-discover"]
-- >     }
-- >   , version = "0.1.0.0"
-- >   , srcs =
-- >     [ 'src/PackageA/A.hs'
-- >     , 'src/PackageA/Other/B.hs'
-- >     , 'src/PackageA/Other/C.hs'
-- >     ]
-- >   , hiddenModules = Just ("PackageA.Other.B" :| ["PackageA.Other.D"])
-- >   , dataAttr = Nothing
-- >   , mainFile = Nothing
-- expressions when assigning a value.
-- >   , privateAttrs = [ ("internal_library", "true") ]
-- >   }
--
-- stands for part of the rule instantiation
--
-- > haskell_library(
-- >   name = 'foo',
-- >   srcs = [
-- >     'src/PackageA/A.hs',
-- >     'src/PackageA/Other/B.hs',
-- >     'src/PackageA/Other/C.hs',
-- >   ],
-- >   ghcopts = ["-Werror", "-Wall"],
-- >   hiddenModules = ["PackageA.Other.B", PackageA.Other.C"],
-- >   deps = ["@stackage//:protolude", "@libsodium//:libsodium"],
-- >   tools = ["@stackage-exe//tasty-discover"],
-- >   version = "0.1.0.0",
-- > )
--
data PackageOutput = PackageOutput
  { flags :: [Flag]
  , rules :: [RuleInfo]
  , configSettingGroups :: [ConfigSettingGroup]
  }

data ConfigSettingGroup = ConfigSettingGroup
  { groupName :: String
  , matchAll :: [String]
  } deriving (Eq, Show)

instance Semigroup PackageOutput where
  (<>) a b = PackageOutput
    { flags = flags a <> flags b
    , rules = rules a <> rules b
    , configSettingGroups = configSettingGroups a <> configSettingGroups b
    }

data RuleInfo = RuleInfo
  { kind :: String
  , name :: String
  , cabalFile :: String
  , importData :: ImportData
  , version :: String
  , srcs :: [ConfigurableList FilePath]
  , hiddenModules :: Maybe (NonEmpty (ConfigurableList String))
  , dataAttr :: Maybe (NonEmpty String)
  , mainFile :: Maybe String
  , privateAttrs :: Attributes
  , ruleConfigGroups :: [ConfigSettingGroup]
  }

data RuleData = RuleData
  { rDeps :: [String]
  , rGhcOpts :: [String]
  , rExtraLibraries :: [String]
  , rTools :: [ToolName]
  , rSrcs :: [FilePath]
  , rHiddenModules :: [String]
  } deriving (Eq, Show)

instance Semigroup RuleData where
  (<>) a b = RuleData {
    rDeps = rDeps a <> rDeps b,
    rGhcOpts = rGhcOpts a <> rGhcOpts b,
    rExtraLibraries = rExtraLibraries a <> rExtraLibraries b,
    rTools = rTools a <> rTools b,
    rSrcs = rSrcs a <> rSrcs b,
    rHiddenModules = rHiddenModules a <> rHiddenModules b
  }

instance Monoid RuleData where
  mempty = RuleData [] [] [] [] [] []

data RuleDataConf = RuleDataConf
  { confDeps :: [ConfigurableList String]
  , confGhcOpts :: [ConfigurableList String]
  , confExtraLibraries :: [ConfigurableList String]
  , confTools :: [ConfigurableList ToolName]
  , confSrcs :: [ConfigurableList FilePath]
  , confHiddenModules :: [ConfigurableList String]
  }

data Flag = Flag { flag :: String, defaultValue :: Bool }

instance Json.JSON Flag where
  showJSON (Flag {..}) = Json.makeObj
      [ ("name", Json.showJSON flag),
        ("defaultValue", Json.showJSON defaultValue) ]

  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

-- Represents a *configurable* build attribute, commonly known as `select()`.
--
-- Note: we're always dealing with `Configurable [a]` (aka `ConfigurableList a`) here,
--       so we can use the shape of data (object or list) to distinguish simple values
--       and select expression when encoding / decoding to JSON.
data Configurable a
  = Value a
  | Select
  { conditionals :: Map String a
  , optionalDefault :: Maybe a
  }
  deriving (Eq, Foldable)

type ConfigurableList a = Configurable [a]

defaultLabel :: String
defaultLabel = "//conditions:default"

instance Show a => Show (Configurable a) where
  show (Value a) = show a
  show (Select m e) =
    "\nselect({\n" ++ Map.foldrWithKey (\ k a b -> b ++ "    \"" ++ k ++ "\": " ++ show a ++ ",\n") "" m ++
      maybe "" (("    \"//conditions:default\": " ++) . show) e ++
      "\n})"

instance Json.JSON a => Json.JSON (Configurable a) where
  showJSON (Value s) = Json.showJSON s
  showJSON (Select m d) = Json.makeObj $
    (Map.map Json.showJSON m & Map.toList) <> maybe [] (\d' -> [(defaultLabel, Json.showJSON d')]) d

  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

instance Functor Configurable where
  fmap :: (a -> b) -> Configurable a -> Configurable b
  fmap f (Value v) = Value $ f v
  fmap f (Select m d) = Select (f <$> m) (f <$> d)

-- | Attributes relevant for dependency resolution
data ImportData = ImportData
  { deps :: [ConfigurableList String]
  , ghcOpts :: [ConfigurableList String]
  , extraLibraries :: [ConfigurableList String]
  , tools :: [ConfigurableList ToolName]
  }

instance Semigroup ImportData where
  (<>) a b =
    a {
      deps = deps a <> deps b,
      ghcOpts = ghcOpts a <> ghcOpts b,
      extraLibraries = extraLibraries a <> extraLibraries b,
      tools = tools a <> tools b
      }

data AttrValue
  = StringListValue [String]
  | StringValue String

data ToolName = ToolName { package :: String, executable :: String } deriving (Eq, Show)

data ComponentType = LIB | EXE | TEST | BENCH
  deriving (Eq, Ord, Show)

type Attributes = [(String, AttrValue)]

instance Json.JSON PackageOutput where
  showJSON (PackageOutput flags rules configGroups) =
    Json.JSObject $ Json.toJSObject
      [ ("flags", Json.showJSON flags)
      , ("rules", Json.showJSON rules)
      , ("config_setting_groups", Json.showJSON configGroups)
      ]
  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

instance Json.JSON ConfigSettingGroup where
  showJSON (ConfigSettingGroup name matchAll) =
    Json.JSObject $ Json.toJSObject
      [ ("name", Json.showJSON name)
      , ("match_all", Json.showJSON matchAll)
      ]
  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

instance Json.JSON RuleInfo where
  showJSON (RuleInfo kind name cabalFile importData version srcs hiddenModules dataAttr mainFile privAttrs _) =
    Json.JSObject $ Json.toJSObject (
      [ ("kind", Json.showJSON kind)
      , ("name", Json.showJSON name)
      , ("srcs", Json.showJSON srcs)
      , ("cabalFile", Json.showJSON cabalFile)
      , ("importData", Json.showJSON importData)
      , ("attrs", attrsJson)
      , ("privateAttrs", attrsToJson privAttrs)] ++
      [("hidden_modules", Json.showJSON xs) | Just xs <- [hiddenModules]])
   where
    attrsToJson as = Json.JSObject $ Json.toJSObject [ (k, Json.showJSON v) | (k, v) <- as ]
    attrsJson =
      Json.JSObject $ Json.toJSObject $
        [ ("version", Json.showJSON version)
        ] ++
        [("data", Json.showJSON xs) | Just xs <- [dataAttr]] ++
        [("main_file", Json.showJSON mf) | Just mf <- [mainFile]]
  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

instance Json.JSON ImportData where
  showJSON (ImportData deps ghcOpts extraLibraries tools) =
    Json.JSObject $ Json.toJSObject
      [ ("deps", Json.showJSON deps)
      , ("ghcopts", Json.showJSON ghcOpts)
      , ("tools", Json.showJSON tools)
      , ("extraLibraries", Json.showJSON extraLibraries)
      ]
  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

instance Json.JSON AttrValue where
  showJSON = \case
    StringValue t -> Json.showJSON t
    StringListValue ts -> Json.showJSON ts
  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

instance Json.JSON ToolName where
  showJSON (ToolName pkg exe) =
    Json.JSObject $ Json.toJSObject
      [ ("packageName", Json.showJSON pkg)
      , ("executableName", Json.showJSON exe)
      ]
  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

instance Json.JSON a => Json.JSON (NonEmpty a) where
  showJSON (hd :| tl) = Json.showJSONs (hd : tl)
  readJSON (Json.JSArray []) = error "Cannot construct a NonEmpty from []"
  readJSON (Json.JSArray (hd : tl)) = (:|) <$> Json.readJSON hd <*> mapM Json.readJSON tl
  readJSON _ = error "Unable to read list"
