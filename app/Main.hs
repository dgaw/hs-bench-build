module Main where

import           Lib

import           Data.Semigroup      ((<>))
import           Options.Applicative

-- | Command line options
data Options = Options {
    verbose  :: Bool
  , interval :: Int
  , file     :: FilePath
}

options :: Parser Options
options = Options
      <$> switch
          ( long "verbose"
         <> short 'v'
         <> help "Verbose output (for debugging)" )
      <*> option auto
          ( long "interval"
         <> short 'i'
         <> help "File polling interval in milliseconds"
         <> showDefault
         <> value 100
         <> metavar "MILLISEC" )
      <*> argument str (metavar "LOG_FILE")

main :: IO ()
main = do
  opts <- execParser optsInfo
  readLog (file opts) 0 (interval opts * 1000) defLineCallback defFinishCallback (verbose opts)
  where
    optsInfo = info (options <**> helper)
      ( fullDesc
     <> progDesc "Watches the LOG_FILE and measures the compilation time of each module."
     <> header "Benchmark the compilation time of each Haskell module using stack log files." )
