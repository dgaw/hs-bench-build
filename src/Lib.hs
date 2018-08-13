{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Function
import           Data.List
import           Data.Time
import           Data.Time.Clock.POSIX
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Posix.Files
import           Text.Printf

type FilePosition = Integer
type MicroSeconds = Int
type Store = MVar [Record]

-- | A Record represents a log line with some metadata attached, e.g. time
data Record = Record {
    recType    :: RecordType
  , recTime    :: UTCTime
  , recContent :: BS.ByteString
  }

data RecordType = GenericLine -- ^ A line we are not interested in but keep around anyway
                | Start -- ^ Start time of log parsing
                | End -- ^ End time of log parsing
                | Module -- ^ Log line about compiling a module

readLog
  :: FilePath
  -> FilePosition
  -> MicroSeconds
  -> (Store -> BS.ByteString -> IO ()) -- ^ Line callback
  -> IO ()
readLog path initSize delay callback = do
  putStrLn $ "Checking the log file every " ++ show (delay `div` 1000) ++ " ms. Run your stack build now."
  st <- initStore
  go initSize st
  where
    initStore = do
      t <- getCurrentTime
      newMVar [Record Start t "Started recording"]
    waitDelay = do
      threadDelay delay
    go sizeSoFar store = do
      startTime <- (recTime . head) <$> readMVar store
      errorOrStat <- tryJust (guard . isDoesNotExistError) $ getFileStatus path
      case errorOrStat of
       Left _ -> do putStrLn "File doesn't exist. Waiting for it..."
                    waitDelay
                    go sizeSoFar store
       Right stat -> do
         let newSize = fromIntegral $ fileSize stat :: Integer
             -- TODO: figure out a better way to detect when to quit
             shouldQuit ls = (posixSecondsToUTCTime $ modificationTimeHiRes stat) > startTime &&
                              length (filter ("Linking" `BS.isPrefixOf`) ls) > 0
         case compare newSize sizeSoFar of
           GT -> do
              -- putStrLn "File changes detected (new lines added)"
              h <- openFile path ReadMode
              hSeek h AbsoluteSeek sizeSoFar
              newContents <- BS.hGetContents h
              let ls = BS.splitWith (==10) newContents
              let startNext = newSize - (toInteger $ BS.length $ last ls)
              mapM_ (callback store) $ init ls
              when (shouldQuit ls) $ do
                t <- getCurrentTime
                xs <- takeMVar store
                putMVar store (xs ++ [Record End t "Ended recording"]) -- ^ Important, see timeModules
                timeModules store >>= prettyPrint
                exitSuccess
              -- hClose h -- TODO: check how it alters behaviour
              waitDelay
              go startNext store
           LT -> do
              -- putStrLn "New file detected - restarting"
              store' <- initStore
              go 0 store'
           EQ -> do
              -- putStrLn "No file change. Waiting..."
              waitDelay
              go sizeSoFar store

lineCallback :: Store -> BS.ByteString -> IO ()
lineCallback store line = do
  now <- getCurrentTime
  xs <- takeMVar store
  let rec = case parseModuleName line of
              Just modName -> Record Module now modName
              Nothing      -> Record GenericLine now line
  putMVar store (xs ++ [rec])

parseModuleName :: BS.ByteString -> Maybe BS.ByteString
parseModuleName x =
  if BS.isInfixOf "Compiling" x
     then Just $ BS.takeWhile (/= 32) $ BS.drop 10 $ snd $ BS.breakSubstring "Compiling" x
     else Nothing

moduleRecords :: [Record] -> [Record]
moduleRecords xs = filter isModule xs
  where
    isModule rec = case recType rec of
                      Module -> True
                      _      -> False

endRecord :: [Record] -> Maybe Record
endRecord xs = case recType (last xs) of
                      End -> Just $ last xs
                      _   -> Nothing

timeModules :: Store -> IO [(BS.ByteString, Double)]
timeModules store = do
  recs <- readMVar store
  let mModulesWithEnd = do
        endRec <- endRecord recs
        return $ moduleRecords recs ++ [endRec]
      in return $ case mModulesWithEnd of
                    Just modulesWithEnd -> go modulesWithEnd []
                    _ -> fail "No end record or no module compilation records found, was the log interrupted?"
  where go [] res = res
        go [_] res = res -- ^ Only "End recording" record
        go (x:y:rest) res = let
           diff = realToFrac (diffUTCTime (recTime y) (recTime x)) :: Double
           in go (y:rest) (res ++ [(recContent x, diff)])

prettyPrint :: [(BS.ByteString, Double)] -> IO ()
prettyPrint times = do
  putStrLn "Biggest offenders at the top:"
  putStrLn "-----------------------------"
  mapM_ (\x -> printf "%s: %0.3f sec \n" (C8.unpack $ fst x) (snd x)) (descending times)
  where descending xs = sortBy (flip compare `on` snd) xs
