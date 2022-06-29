module CabalScan.Options (parseCommandLine) where

import System.Environment
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified System.IO as SIO
import qualified System.Exit as Exit
import qualified System.Console.GetOpt as Opt

data InputOptions = Help

type CabalFile = FilePath
type CabalFiles = NonEmpty CabalFile

options :: [ Opt.OptDescr InputOptions ]
options = [ Opt.Option ['h'] ["help"] (Opt.NoArg Help) "Prints this message" ]

parseArgs :: [String] -> IO CabalFiles
parseArgs argv = case Opt.getOpt Opt.Permute options argv of
                     (_:_,_,_) -> hPutLn SIO.stdout [usage] *> Exit.exitSuccess
                     (_,[],_)  -> hPutLn SIO.stderr [missingFiles, usage] *> Exit.exitFailure
                     (_,h:t,_) -> return (h:|t)
  where usage :: String
        usage = Opt.usageInfo header options
        header :: String
        header = unlines ["Usage: cabalscan [OPTION...] CABAL_FILES...",
                          "  Prints in stdout information extracted from Cabal files in JSON format."]
        missingFiles :: String
        missingFiles = "Missing: CABAL_FILES..."
        hPutLn :: SIO.Handle -> [String] -> IO ()
        hPutLn h = SIO.hPutStrLn h . intercalate "\n\n"

parseCommandLine :: IO CabalFiles
parseCommandLine = getArgs >>= parseArgs
