{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import CabalScan.Rules (RuleInfo)
import CabalScan.RuleGenerator (generateRulesForCabalFile)
import qualified CabalScan.Options as Options
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy

main :: IO ()
main = do
  cabalFiles <- Options.parseCommandLine
  mapM generateRulesForCabalFile cabalFiles
    >>= printRuleInfos . concat

printRuleInfos :: [RuleInfo] -> IO ()
printRuleInfos = ByteString.Lazy.putStrLn . Aeson.encode
