{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import CabalScan.Rules (RuleInfo)
import CabalScan.RuleGenerator (generateRulesForCabalFile)
import qualified CabalScan.Options as Options
import qualified Text.JSON as Json

main :: IO ()
main = do
  cabalFiles <- Options.parseCommandLine
  mapM generateRulesForCabalFile cabalFiles
    >>= printRuleInfos . concat

printRuleInfos :: [RuleInfo] -> IO ()
printRuleInfos = putStrLn . Json.encode
