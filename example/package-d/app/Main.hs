{-#LANGUAGE CPP #-}

module Main (main) where

import PackageD.Core

#ifdef NETWORK_SUPPORT
import PackageD.Network
#endif

main :: IO ()
main = do
  putStrLn $ greet "World"
#ifdef NETWORK_SUPPORT
  result <- fetchData "example.com"
  putStrLn result
#endif
