module CabalScan.Options (parseCommandLine) where

import System.Environment
import Data.List (intercalate)
import qualified Path as Path
import qualified System.IO as SIO
import qualified System.Exit as Exit
import qualified System.Console.GetOpt as Opt

data InputOptions = Help

type CabalFiles = [Path.Path Path.Abs Path.File]

options :: [ Opt.OptDescr InputOptions ]
options = [ Opt.Option ['h'] ["help"] (Opt.NoArg Help) "Prints this message" ]

parseArgs :: [String] -> IO CabalFiles
parseArgs argv = case Opt.getOpt Opt.Permute options argv of
                     (_:_,_,_)   -> hPutLn SIO.stdout [usage] *> Exit.exitSuccess
                     (_,[],_)    -> hPutLn SIO.stderr [missingFiles, usage] *> Exit.exitFailure
                     (_,files,_) -> sequence [ maybe (logErrAndExit f) return (Path.parseAbsFile f) | f <- files ]
  where usage = Opt.usageInfo header options
        header = unlines ["Usage: cabalscan [OPTION...] CABAL_FILES...",
                          "  Prints in stdout information extracted from Cabal files in JSON format."]
        missingFiles = "Missing: CABAL_FILES..."
        wrongpath f = "Couldn't parse absolute file path: '" ++ f ++ "'"
        logErrAndExit f = hPutLn SIO.stderr [wrongpath f, usage] *> Exit.exitFailure
        hPutLn :: SIO.Handle -> [String] -> IO ()
        hPutLn h = SIO.hPutStrLn h . intercalate "\n\n"

parseCommandLine :: IO CabalFiles
parseCommandLine = getArgs >>= parseArgs
