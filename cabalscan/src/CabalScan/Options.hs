{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalScan.Options
  ( Options(..)
  , parseCommandLine
  ) where

import Path (Abs, File, Path, parseAbsFile)
import System.Environment
import System.Console.GetOpt
import System.Exit (exitFailure, exitSuccess)
import Data.List (intercalate)
import System.IO (hPutStrLn, stderr, stdout, Handle)

data InputOptions = Help

options :: [ OptDescr InputOptions ]
options = [ Option ['h'] ["help"] (NoArg Help) "Prints this message" ]

data Options = Options
  { cabalFiles :: [Path Abs File]
  }

parseArgs :: [String] -> IO [Path Abs File]
parseArgs argv = case getOpt Permute options argv of
                     (_:_,_,_)   -> hPutLn stdout [usage] *> exitSuccess
                     (_,[],_)    -> hPutLn stderr [missingFiles, usage] *> exitFailure
                     (_,files,_) -> sequence [ maybe (logErrAndExit f) return (parseAbsFile f) | f <- files ]
  where usage = [usageInfo header options]
        header = "Usage: cabalscan [OPTION...] CABAL_FILES..."
        missingFiles = ["Missing: CABAL_FILES..."]
        wrongpath f = ["Couldn't parse absolute file path: " ++ f]
        logErrAndExit f = hPutLn stderr [wrongpath f, usage] *> exitFailure
        hPutLn :: Handle -> [[String]] -> IO ()
        hPutLn h = hPutStrLn h . intercalate "\n" . map unlines

parseCommandLine :: IO Options
parseCommandLine = getArgs >>= parseArgs >>= return . Options
