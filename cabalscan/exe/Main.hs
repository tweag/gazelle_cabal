module Main where

import CabalScan.Rules (PackageOutput(..))
import CabalScan.RuleGenerator (generateRulesForCabalFile)
import qualified CabalScan.Options as Options
import qualified Text.JSON as Json
import Data.Semigroup (sconcat)

main :: IO ()
main = do
  cabalFiles <- Options.parseCommandLine
  outputs <- mapM generateRulesForCabalFile cabalFiles
  printPackageOutput (sconcat outputs)

printPackageOutput :: PackageOutput -> IO ()
printPackageOutput = putStrLn . Json.encode
