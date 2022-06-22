{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CabalScan.Rules where

import qualified Text.JSON as Json
import Data.List.NonEmpty (NonEmpty((:|)))

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
data RuleInfo = RuleInfo
  { kind :: String
  , name :: String
  , cabalFile :: String
  , importData :: ImportData
  , version :: String
  , srcs :: [String]
  , hiddenModules :: Maybe (NonEmpty String)
  , dataAttr :: Maybe (NonEmpty String)
  , mainFile :: Maybe String
  , privateAttrs :: Attributes
  }

-- | Attributes relevant for dependency resolution
data ImportData = ImportData
  { deps :: [String]
  , ghcOpts :: [String]
  , extraLibraries :: [String]
  , tools :: [ToolName]
  }

data AttrValue
  = StringListValue [String]
  | StringValue String

data ToolName = ToolName { package :: String, executable :: String }

data ComponentType = LIB | EXE | TEST | BENCH
  deriving (Eq, Ord, Show)

type Attributes = [(String, AttrValue)]

instance Json.JSON RuleInfo where
  showJSON (RuleInfo kind name cabalFile importData version srcs hiddenModules dataAttr mainFile privAttrs) =
    Json.JSObject $ Json.toJSObject
      [ ("kind", Json.showJSON kind)
      , ("name", Json.showJSON name)
      , ("cabalFile", Json.showJSON cabalFile)
      , ("importData", Json.showJSON importData)
      , ("attrs", attrsJson)
      , ("privateAttrs", attrsToJson privAttrs)
      ]
   where
    attrsToJson as = Json.JSObject $ Json.toJSObject [ (k, Json.showJSON v) | (k, v) <- as ]
    attrsJson =
      Json.JSObject $ Json.toJSObject $
        [ ("version", Json.showJSON version)
        , ("srcs", Json.showJSON srcs )
        ] ++
        [("hidden_modules", Json.showJSON xs) | Just xs <- [hiddenModules]] ++
        [("data", Json.showJSON xs) | Just xs <- [dataAttr]] ++
        [("main_file", Json.showJSON mf) | Just mf <- [mainFile]]
  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

instance Json.JSON ImportData where
  showJSON (ImportData deps ghcOpts extraLibraries tools) =
    Json.JSObject $ Json.toJSObject
      [ ("deps", Json.showJSON deps)
      , ("ghcopts", Json.showJSON ghcOpts)
      , ("tools", Json.showJSON tools)
      , ("extraLibraries", Json.showJSON (StringListValue extraLibraries))
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
