{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Cabal2Build.Rules (RuleInfo)
import Cabal2Build.RuleGenerator (generateRulesForCabalFile)
import qualified Cabal2Build.Options as Options
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy

main :: IO ()
main = do
  opts <- Options.parseCommandLine
  mapM generateRulesForCabalFile (Options.cabalFiles opts)
    >>= printRuleInfos . concat

printRuleInfos :: [RuleInfo] -> IO ()
printRuleInfos =
  ByteString.Lazy.putStrLn . Aeson.encode . Aeson.toJSON
