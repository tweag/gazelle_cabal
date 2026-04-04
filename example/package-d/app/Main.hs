{-#LANGUAGE CPP #-}

module Main (main) where

import PackageD.Core

#ifdef NETWORK_SUPPORT
import PackageD.Network
#endif

main :: IO ()
main = do
  putStrLn "Package-D Configuration:"
  putStrLn "========================"
  
  putStrLn $ "Core: " ++ greet "enabled"
  
#ifdef NETWORK_SUPPORT
  putStrLn "Network: enabled"
  result <- fetchData "example.com"
  putStrLn $ "  - " ++ result
#else
  putStrLn "Network: disabled"
#endif

#ifdef DATABASE_SUPPORT
  putStrLn "Database: enabled"
#else
  putStrLn "Database: disabled"
#endif

#ifdef DATABASE_AND_NETWORK_SUPPORT
  putStrLn "Network+Database: enabled"
#else
  putStrLn "Network+Database: disabled"
#endif
