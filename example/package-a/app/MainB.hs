{-# LANGUAGE CPP #-}
module Main where

import PackageA.Other.C()

main :: IO ()
main = do
  putStrLn VERSION_package_a
  print (SOME_VALUE :: Int)
