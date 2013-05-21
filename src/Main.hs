module Main where

import Voxy
import System.Console.GetOpt
import System.Environment
import System.FilePath          ( (</>) )


main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (  [], _,   []) -> error $ usageInfo header options
        (opts, _,   []) -> startWithOpts $ foldl (flip id) defaultOptions opts
        (   _, _, msgs) -> error $ concat msgs ++ usageInfo header options

header :: String
header = "Usage: voxy -d data_dir"

data Options = Options { optDataDir :: FilePath } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { optDataDir = "." </> "data" }

options :: [OptDescr (Options -> Options)]
options = [ Option "d" ["data directory"]
              (ReqArg (\d opts -> opts { optDataDir = d }) "DATA_DIRECTORY")
              "set the directory to read/write data from"
          ]

startWithOpts :: Options -> IO ()
startWithOpts opts = voxy $ optDataDir opts 
