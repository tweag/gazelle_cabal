{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CabalScan.Rules where

import qualified Data.Aeson as Aeson
import Data.Text (Text)

-- | Information about rules to give to bazel
--
-- > RuleInfo
-- >   { kind = "haskell_library"
-- >   , name = "foo"
-- >   , deps = ["protolude"]
-- >   , extraLibraries = ["libsodium"]
-- >   , tools = [ToolName "tasty-discover" "tasty-discover"]
-- >   , attrs = [ ("srcs", ["src/Main.hs"])
-- >             , ("compiler_flags", "-DFOO=1")
-- >             ]
-- >   }
--
-- stands for the rule instantiation
--
-- > haskell_library(
-- >   name = 'foo',
-- >   srcs = ['src/Main.hs'],
-- >   deps = ["@stackage//:protolude", "@libsodium//:libsodium"]
-- >   tools = ["@stackage-exe//tasty-discover"]
-- > )
--
data RuleInfo = RuleInfo
  { kind :: Text
  , name :: Text
  , cabalFile :: Text
  , importData :: ImportData
  , attrs :: [(Text, AttrValue)]
  }

-- | Attributes relevant for dependency resolution
data ImportData = ImportData
  { deps :: [Text]
  , compilerFlags :: [Text]
  , extraLibraries :: [Text]
  , tools :: [ToolName]
  }

data AttrValue
  = StringListValue [Text]
  | TextValue Text

data ToolName = ToolName { package :: Text, executable :: Text }

data ComponentType = LIB | EXE | TEST | BENCH
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON RuleInfo where
  toJSON (RuleInfo kind name cabalFile importData attrs) =
    Aeson.object
      [ ("kind", Aeson.String kind)
      , ("name", Aeson.String name)
      , ("cabalFile", Aeson.String cabalFile)
      , ("importData", Aeson.toJSON importData)
      , ("attrs", attrsObj)
      ]
   where
    attrsObj = Aeson.object [ (k, Aeson.toJSON v) | (k, v) <- attrs ]

instance Aeson.ToJSON ImportData where
  toJSON (ImportData deps compilerFlags extraLibraries tools) =
    Aeson.object
      [ ("deps", Aeson.toJSON deps)
      , ("compilerFlags", Aeson.toJSON compilerFlags)
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
