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
import System.IO (hPutStrLn, stderr, stdout, Handle)

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
  MissingFiles -> hPutLn stderr [missing, usage] *> exitFailure
  WrongFilePath f -> hPutLn stderr [wrongpath f, usage] *> exitFailure
  PrintHelp -> hPutLn stdout [info, usage, options] *> exitSuccess
  where
    info = ["cabalscan - extract build information from Cabal files"]
    usage = ["Usage: cabalscan CABAL_FILES...",
             "  Prints in stdout information extracted from Cabal files in JSON format."]
    options = ["Available options:",
               "-h,--help                Show this help text"]
    missing = ["Missing: CABAL_FILES..."]
    wrongpath p = ["Couldn't parse absolute file path: " ++ p]
    hPutLn :: Handle -> [[String]] -> IO ()
    hPutLn h = hPutStrLn h . intercalate "\n" . map unlines
