module Main where

import           Lib

import           System.Environment
import           System.Exit

usage :: String
usage = "Benchmark the compilation time of each Haskell module using the stack log file.\n"
        ++ "Usage: hs-bench-build .stack-work/logs/myapp-0.0.1.log"

pollInterval :: MicroSeconds
pollInterval = 100 * 1000

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"] -> do
            putStrLn usage
            exitSuccess
        ["--help"] -> do
            putStrLn usage
            exitSuccess
        [path] -> do
            readLog path 0 pollInterval lineCallback
        _ -> do
            putStrLn usage
            exitFailure
