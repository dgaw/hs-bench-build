{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time
-- import           Data.Time.Clock.POSIX
import           System.Exit
import           System.IO
import           System.IO             (isEOF)
import           System.IO.Error
import           System.Posix.Files
import           Text.Printf

type FilePosition = Integer
type MicroSeconds = Int
type Store = MVar [Record]
type Verbose = Bool

-- | A Record represents a log line with some metadata attached, e.g. time
data Record = Record {
    recType    :: RecordType
  , recTime    :: UTCTime
  , recContent :: BS.ByteString
} deriving Show

data RecordType = GenericLine -- ^ A log line
                | End         -- ^ Extra pseudo line used to time the last log line
                | Module      -- ^ A line corresponding to compiling a module
                deriving Show

-- | Reads the stdin for log lines or (if provided) polls the log file
--   Calls the lineCallback on each new line.
--   Calls the finishCallback when it encounters the linking stage in the compilation.
readLog
  :: Maybe FilePath
  -> FilePosition
  -> MicroSeconds
  -> (Store -> BS.ByteString -> IO ()) -- ^ Line callback
  -> (Store -> IO ())                  -- ^ Finish callback
  -> Verbose
  -> IO ()
readLog mPath initSize delay lineCallback finishCallback verbose = do
  st <- initStore
  case mPath of
    Just path -> do
      putStrLn $ "Checking the log file every " ++ show (delay `div` 1000) ++ " ms..."
      goFile path initSize st
    Nothing -> do
      putStrLn "Reading log lines from stdin..."
      goStdin st

  where
    initStore = newMVar []
    waitDelay = threadDelay delay

    goStdin store = do
      -- startTime <- (recTime . head) <$> readMVar store
      done <- isEOF
      if done
         then do
           finishCallback store >> exitSuccess
         else do
           l <- getLine
           lineCallback store $ C8.pack l
           goStdin store

    goFile path sizeSoFar store = do
      -- startTime <- (recTime . head) <$> readMVar store
      errorOrStat <- tryJust (guard . isDoesNotExistError) $ getFileStatus path
      case errorOrStat of
       Left _ -> do putStrLn "Error: file doesn't exist."
                    exitFailure
       Right stat -> do
         let newSize = fromIntegral $ fileSize stat :: Integer
             -- TODO: figure out a better way to detect when to quit
             -- shouldQuit ls = (posixSecondsToUTCTime $ modificationTimeHiRes stat) > startTime &&
             --                  length (filter ("Linking" `BS.isPrefixOf`) ls) > 0
         case compare newSize sizeSoFar of
           GT -> do
              when verbose $ putStrLn "File changes detected (new lines added)" -- Debug
              h <- openFile path ReadMode
              hSeek h AbsoluteSeek sizeSoFar
              newContents <- BS.hGetContents h
              let ls = BS.splitWith (==10) newContents
              let startNext = newSize - (toInteger $ BS.length $ last ls)
              mapM_ (lineCallback store) $ init ls
              done <- isEOF -- EOF of stdin, not the file
              when done $ finishCallback store >> exitSuccess
              -- hClose h -- TODO: check how it alters behaviour
              waitDelay
              goFile path startNext store
           LT -> do
              when verbose $ putStrLn "New file detected - restarting" -- Debug
              store' <- initStore
              goFile path 0 store'
           EQ -> do
              -- when verbose $ putStrLn "No file change. Waiting..." -- Debug (noisy)
              waitDelay
              goFile path sizeSoFar store

-- | Called on every new line in the log
--   Create a Record for the line and put it in the Store
defLineCallback :: Store -> BS.ByteString -> IO ()
defLineCallback store line = do
  now <- getCurrentTime
  xs <- takeMVar store
  let rec = if isModule line
              then Record Module now line
              else Record GenericLine now line
  putMVar store (xs ++ [rec])
  where
    isModule l = BS.isInfixOf "Compiling" l

-- | Called at the end of log reading
--   Adds the "End record" and outputs the results
defFinishCallback :: Store -> IO ()
defFinishCallback store = do
  t <- getCurrentTime
  recs <- takeMVar store
  let recsWithEnd = (recs ++ [Record End t "Ended recording"]) -- ^ Important, see timeRecords
      in do
        putMVar store (recsWithEnd)
        case timeRecords recsWithEnd of
          Left e              -> fail e
          Right recsWithTimes -> prettyPrint recsWithTimes

-- | Get the "End record" (used for timing)
endRecord :: [Record] -> Maybe Record
endRecord xs = case recType (last xs) of
                      End -> Just $ last xs
                      _   -> Nothing

-- | Calculate the time each record took in the build
timeRecords :: [Record] -> Either String [(Record, Double)]
timeRecords recs = do
  if (isNothing $ endRecord recs)
     then Left "timeRecords: No End record found"
     else Right $ go recs []
  where go [] res = res
        go [_] res = res -- ^ End record
        go (x:y:rest) res = let
           diff = realToFrac (diffUTCTime (recTime y) (recTime x)) :: Double
           in go (y:rest) (res ++ [(x, diff)])

-- | Pretty print the results of timeRecords
prettyPrint :: [(Record, Double)] -> IO ()
prettyPrint (first:rest) = do
  putStrLn "-----------------------------"
  putStrLn "Biggest offenders at the top:"
  putStrLn "-----------------------------"
  mapM_ (\x -> printf "%0.2f sec: %s\n" (snd x) (C8.unpack $ prettyRec $ fst x)) descendingTimes
  where descendingTimes = sortBy (flip compare `on` snd) (first:rest)
prettyPrint [] = putStrLn "No log lines found"

-- | Pretty print an invidual record
prettyRec :: Record -> BS.ByteString
-- Module line:
--   "[26 of 28] Compiling Web.Views.Site   ( myapp/Web/Views/Site.hs ..."
prettyRec (Record Module _ content) =
  "Compiling " <> (BS.takeWhile (/= 32) $ BS.drop 10 $ snd $ BS.breakSubstring "Compiling" content)
prettyRec (Record _ _ content) = ellipsis (BS.take 70 content)
  where ellipsis x = if BS.last x == 46 -- full stop, dot
                        then x
                        else x <> "..."
