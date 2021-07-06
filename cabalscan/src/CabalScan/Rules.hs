{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cabal2Build.Rules where

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
  , deps :: [Text]
  , compilerFlags :: [Text]
  , extraLibraries :: [Text]
  , tools :: [ToolName]
  , attrs :: [(Text, AttrValue)]
  }

data AttrValue
  = StringListValue [Text]
  | TextValue Text

data ToolName = ToolName { package :: Text, executable :: Text }

data ComponentType = LIB | EXE | TEST | BENCH
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON RuleInfo where
  toJSON (RuleInfo kind name deps compilerFlags extraLibraries tools attrs) =
    Aeson.object
      [ ("kind", Aeson.String kind)
      , ("name", Aeson.String name)
      , ("importData", importData)
      , ("attrs", attrsObj)
      ]
   where
    attrsObj = Aeson.object [ (k, Aeson.toJSON v) | (k, v) <- attrs ]

    importData = Aeson.object
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
