{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CabalScan.Rules where

import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

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
-- >   , version = "0.1.0.0"]
-- >   , hidden_modules = Just ("PackageA.Other.B" :| ["PackageA.Other.D"])
-- >   , dataAttr = Nothing
-- >   , main_file = Nothing
-- >   , privateAttrs = [ ("internal_library", "true") ]
-- >   }
--
-- stands for part of the rule instantiation
--
-- > haskell_library(
-- >   name = 'foo',
-- >   srcs = ['src/Main.hs'],
-- >   ghcopts = ["-Werror", "-Wall"],
-- >   hidden_modules = ["PackageA.Other.B", PackageA.Other.D"],
-- >   deps = ["@stackage//:protolude", "@libsodium//:libsodium"],
-- >   tools = ["@stackage-exe//tasty-discover"],
-- >   version = "0.1.0.0",
-- > )
--
data RuleInfo = RuleInfo
  { kind :: Text
  , name :: Text
  , cabalFile :: Text
  , importData :: ImportData
  , version :: Text
  , srcs :: [Text]
  , hidden_modules :: Maybe (NonEmpty Text)
  , dataAttr :: Maybe (NonEmpty Text)
  , main_file :: Maybe Text
  , privateAttrs :: Attributes
  }

-- | Attributes relevant for dependency resolution
data ImportData = ImportData
  { deps :: [Text]
  , ghcOpts :: [Text]
  , extraLibraries :: [Text]
  , tools :: [ToolName]
  }

data AttrValue
  = StringListValue [Text]
  | TextValue Text

data ToolName = ToolName { package :: Text, executable :: Text }

data ComponentType = LIB | EXE | TEST | BENCH
  deriving (Eq, Ord, Show)

type Attributes = [(Text, AttrValue)]

instance Aeson.ToJSON RuleInfo where
  toJSON (RuleInfo kind name cabalFile importData version srcs hidden_modules dataAttr main_file privAttrs) =
    Aeson.object
      [ ("kind", Aeson.String kind)
      , ("name", Aeson.String name)
      , ("cabalFile", Aeson.String cabalFile)
      , ("importData", Aeson.toJSON importData)
      , ("attrs", attrsJson)
      , ("privateAttrs", attrsToJson privAttrs)
      ]
   where
    attrsToJson as = Aeson.object [ (k, Aeson.toJSON v) | (k, v) <- as ]
    attrsJson =
      Aeson.object $
        [ ("version", Aeson.String version)
        , ("srcs", Aeson.toJSON srcs )
        ] ++
        [("hidden_modules", Aeson.toJSON xs) | Just xs <- [hidden_modules]] ++
        [("data", Aeson.toJSON xs) | Just xs <- [dataAttr]] ++
        [("main_file", Aeson.String mf) | Just mf <- [main_file]]

instance Aeson.ToJSON ImportData where
  toJSON (ImportData deps ghcOpts extraLibraries tools) =
    Aeson.object
      [ ("deps", Aeson.toJSON deps)
      , ("ghcopts", Aeson.toJSON ghcOpts)
      , ("tools", Aeson.toJSON tools)
      , ("extraLibraries", Aeson.toJSON (StringListValue extraLibraries))
      ]

instance Aeson.ToJSON AttrValue where
  toJSON = \case
    TextValue t -> Aeson.String t
    StringListValue ts -> Aeson.toJSON ts

instance Aeson.ToJSON ToolName where
  toJSON (ToolName pkg exe) =
    Aeson.object
      [ ("packageName", Aeson.toJSON pkg)
      , ("executableName", Aeson.toJSON exe)
      ]
