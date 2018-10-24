module Hackage.Security.Util.IO (
    -- * Miscelleneous
    getFileSize
  , handleDoesNotExist
  , withDirLock
    -- * Debugging
  , timedIO
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Exception
import Data.Time
import Data.Typeable (Typeable)
import System.IO hiding (openTempFile, withFile)
import System.IO.Error
import Text.Printf

import Hackage.Security.Util.Path
import Hackage.Security.Util.FileLock (hTryLock, LockMode(ExclusiveLock), FileLockingNotSupported)

{-------------------------------------------------------------------------------
  Miscelleneous
-------------------------------------------------------------------------------}

getFileSize :: (Num a, FsRoot root) => Path root -> IO a
getFileSize fp = fromInteger <$> withFile fp ReadMode hFileSize

handleDoesNotExist :: IO a -> IO (Maybe a)
handleDoesNotExist act =
   handle aux (Just <$> act)
  where
    aux e =
      if isDoesNotExistError e
        then return Nothing
        else throwIO e

-- | Exception thrown by 'withDirLock' when it fails to acquire the lock.
--
-- FIXME: this only currently applies to hTryLock, not to the directory locking
-- fallback.
data LockTaken = LockTaken FilePath
  deriving (Typeable)

instance Show LockTaken where
  show (LockTaken path) = "Could not acquire the lock at " ++ show path

instance Exception LockTaken

-- | Attempt to create a filesystem lock in the specified directory.
--
-- This will use OS-specific file locking primitives: "GHC.IO.Handle.Lock" with
-- @base-4.10" and later or a shim for @base@ versions.
--
-- Throws an exception if the lock is already present.
--
-- May fallback to locking via creating a directory:
-- Given a file @/path/to@, we do this by attempting to create the directory
-- @//path/to/hackage-security-lock@, and deleting the directory again
-- afterwards. Creating a directory that already exists will throw an exception
-- on most OSs (certainly Linux, OSX and Windows) and is a reasonably common way
-- to implement a lock file.
withDirLock :: Path Absolute -> IO a -> IO a
withDirLock dir = bracket takeLock releaseLock . const
  where
    lock :: Path Absolute
    lock = dir </> fragment "hackage-security-lock"

    lock' :: FilePath
    lock' = toFilePath lock

    takeLock = retryLock $ do
        h <- openFile lock' ReadWriteMode
        handle (takeDirLock h) $ do
            gotlock <- hTryLock h ExclusiveLock
            unless gotlock $
                throwIO $ LockTaken lock'
            return (Just h)

    takeDirLock :: Handle -> FileLockingNotSupported -> IO (Maybe Handle)
    takeDirLock h _ = do
        -- We fallback to directory locking
        -- so we need to cleanup lock file first: close and remove
        hClose h
        handle onIOError (removeFile lock)
        createDirectory lock
        return Nothing

    onIOError :: IOError -> IO ()
    onIOError _ = hPutStrLn stderr
        "withDirLock: cannot remove lock file before directory lock fallback"

    releaseLock (Just h) = hClose h
    releaseLock Nothing  = removeDirectory lock

    -- Retry the IO action a few times with progressive backoff
    retryLock :: forall a . IO a -> IO a
    retryLock a = go [1,2,5,60] -- backoff in seconds
      where
        go :: [Int] -> IO a
        go backoff = do
          try a >>= \x -> case x :: Either LockTaken a of
            Right r -> return r
            Left e
              | t:ts <- backoff -> do
                printf "Got exception %s. Waiting for %d seconds, then retrying.\n" (show e) t
                threadDelay (t * 1000000)
                go ts
              | otherwise -> throwIO e


{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

timedIO :: String -> IO a -> IO a
timedIO label act = do
    before <- getCurrentTime
    result <- act
    after  <- getCurrentTime
    hPutStrLn stderr $ label ++ ": " ++ show (after `diffUTCTime` before)
    hFlush stderr
    return result
