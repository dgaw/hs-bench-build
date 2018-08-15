module Main where

import           Lib

import           Data.Semigroup      ((<>))
import           Options.Applicative

-- | Command line options
data Options = Options {
    optVerbose  :: Bool
  , optInterval :: MilliSeconds
  , optFile     :: Maybe FilePath
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
      -- <*> optional $ argument str (metavar "LOG_FILE")
      <*> (optional $ strOption
          ( long "file"
         <> short 'f'
         <> help "Log file (required if using multiple local packages)"
         <> metavar "LOG_FILE") )

main :: IO ()
main = do
  opts <- execParser optsInfo
  readLog (optFile opts) (optInterval opts) defLineCallback defFinishCallback (optVerbose opts)
  where
    optsInfo = info (options <**> helper)
      ( fullDesc
     <> progDesc "Watches stdin or LOG_FILE and measures the build time of each line."
     <> header "Benchmark the compilation time of your Haskell project." )
