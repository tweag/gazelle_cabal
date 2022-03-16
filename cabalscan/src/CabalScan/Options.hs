{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalScan.Options
  ( Options(..)
  , parseCommandLine
  ) where

import Data.List (intercalate)
import Path (Abs, File, Path, parseAbsFile)
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)

type AbsoluteFilepath = Path Abs File

data Options = Options
  { cabalFiles :: [AbsoluteFilepath]
  }

data InvocationError = MissingFiles | WrongFilePath String | PrintHelp

parseCommandLine :: IO Options
parseCommandLine = do
  args <- getArgs
  case parseArgs args [] of
    Right xs -> return (Options {cabalFiles = xs})
    Left err -> printMsgAndExit err

parseArgs :: [String] -> [AbsoluteFilepath] -> Either InvocationError [AbsoluteFilepath]
parseArgs [] [] = Left MissingFiles
parseArgs [] xs = Right xs
parseArgs (x:xs) fs | x `elem` ["-h", "--help"] = Left PrintHelp
                    | otherwise = case parseAbsFile x of
                                    Just f -> parseArgs xs (f:fs)
                                    _      -> Left $ WrongFilePath x

printMsgAndExit :: InvocationError -> IO a
printMsgAndExit = \case
  MissingFiles -> putErrLn [missing, usage] *> exitFailure
  WrongFilePath f -> putErrLn [wrongpath f, usage] *> exitFailure
  PrintHelp -> putErrLn [info, usage, options] *> exitSuccess
  where
    info = unlines ["cabalscan - extract build information from Cabal files"]
    usage = unlines ["Usage: cabalscan CABAL_FILES...",
                     "  Prints in stdout information extracted from Cabal files in JSON format."]
    options = unlines ["Available options:",
                       "-h,--help                Show this help text"]
    missing = unlines ["Missing: CABAL_FILES..."]
    wrongpath p = unlines ["Couldn't parse absolute file path: " ++ p]
    putErrLn :: [String] -> IO ()
    putErrLn = hPutStrLn stderr . intercalate "\n"
