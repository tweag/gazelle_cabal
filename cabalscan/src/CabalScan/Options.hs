module CabalScan.Options (parseCommandLine) where

import System.Environment
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Path as Path
import qualified System.IO as SIO
import qualified System.Exit as Exit
import qualified System.Console.GetOpt as Opt

data InputOptions = Help

type CabalFile = Path.Path Path.Abs Path.File
type CabalFiles = NonEmpty CabalFile

options :: [ Opt.OptDescr InputOptions ]
options = [ Opt.Option ['h'] ["help"] (Opt.NoArg Help) "Prints this message" ]

parseArgs :: [String] -> IO CabalFiles
parseArgs argv = case Opt.getOpt Opt.Permute options argv of
                     (_:_,_,_) -> hPutLn SIO.stdout [usage] *> Exit.exitSuccess
                     (_,[],_)  -> hPutLn SIO.stderr [missingFiles, usage] *> Exit.exitFailure
                     (_,h:t,_) -> traverse validate (h:|t)
  where usage :: String
        usage = Opt.usageInfo header options
        header :: String
        header = unlines ["Usage: cabalscan [OPTION...] CABAL_FILES...",
                          "  Prints in stdout information extracted from Cabal files in JSON format."]
        missingFiles :: String
        missingFiles = "Missing: CABAL_FILES..."
        wrongpath :: FilePath -> String
        wrongpath f = "Couldn't parse absolute file path: '" ++ f ++ "'"
        logErrAndExit :: FilePath -> IO a
        logErrAndExit f = hPutLn SIO.stderr [wrongpath f, usage] *> Exit.exitFailure
        validate :: FilePath -> IO CabalFile
        validate path = maybe (logErrAndExit path) return (Path.parseAbsFile path)
        hPutLn :: SIO.Handle -> [String] -> IO ()
        hPutLn h = SIO.hPutStrLn h . intercalate "\n\n"

parseCommandLine :: IO CabalFiles
parseCommandLine = getArgs >>= parseArgs
