{-# LANGUAGE BlockArguments #-}

import Control.Concurrent
import Control.Monad
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO.Handle
  ( BufferMode (NoBuffering),
    Handle,
    hFlush,
    hGetLine,
    hIsEOF,
    hPutStr,
    hSetBuffering,
  )
import System.Environment (getArgs)
import System.Process
  ( CreateProcess (std_in, std_out),
    StdStream (CreatePipe),
    callCommand,
    createProcess,
    shell,
    spawnCommand,
  )
import System.Process.Internals
  ( PHANDLE,
    ProcessHandle,
    ProcessHandle__ (ClosedHandle, OpenHandle),
    withProcessHandle,
  )

-- | returns Just pid or Nothing if process has already exited
getPid ph = withProcessHandle ph go
  where
    go ph_ = case ph_ of
      OpenHandle x -> return $ Just x
      ClosedHandle _ -> return Nothing

spawnShell :: String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
spawnShell command =
  createProcess
    (shell command)
      { std_out = CreatePipe,
        std_in = CreatePipe
      }

onlyStdout :: (a1, Maybe a2, c, d) -> a2
onlyStdout (_, Just stdout, _, _) = stdout

getCommand :: Show a => Maybe a -> [Char]
getCommand Nothing = ""
getCommand (Just pid) = "ps ax -o pid,rss  | grep " ++ (show pid) ++ "|awk -F\" \" '{print $2}'   | tee ps"

-- flushShell :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> String -> IO String
-- flushShell (Just std_in, Just std_out, _, _) cmd = do
--   hPutStr std_in cmd
--   hFlush std_in
--   hGetLine std_out

-- spawnThenRead s = do
--   shellInfo <- spawnShell s
--   flushShell shellInfo s
(|>) f g = g . f

main = do
  args <- getArgs
  let command = unwords args
  processInfo <-
    spawnShell command
  spawnShell "echo > out.csv"
  case processInfo of
    (Just stdin, Just stdout, _, processHandle) -> do
      let pid = getPid processHandle
      hSetBuffering stdin NoBuffering
      pid >>= print

      mainLoop stdin stdout pid
    _ -> do error "Unable to start process"

appendEndFile :: FilePath -> String -> IO String
appendEndFile file content = do
  x <- readFile file
  length x `seq` writeFile file content
  appendFile file x
  return x

mainLoop :: Handle -> Handle -> IO (Maybe PHANDLE) -> IO ()
mainLoop input output pid =
  do
    itsOver <- hIsEOF output
    if itsOver
      then return ()
      else do
        inpStr <- hGetLine output

        posixTime <- getPOSIXTime
        maybePidStr <- pid
        let command = getCommand maybePidStr
        mem <- awaitShell command
        let content = show posixTime ++ ",\"" ++ inpStr ++ "\"," ++ mem ++ "\n"
        putStrLn content
        appendEndFile "out.csv" content
        mainLoop input output pid

awaitProcess :: Maybe Handle -> IO String
awaitProcess (Just stdout) = accumulateProcess ""
  where
    accumulateProcess actualContent =
      hIsEOF stdout
        >>= ( \isEOF ->
                if isEOF
                  then return actualContent
                  else hGetLine stdout >>= accumulateProcess
            )

andThen :: String -> (String -> a) -> IO a
andThen command fn = do
  processInfo <- spawnShell command
  case processInfo of
    (Just stdin, stdout, _, processHandle) -> do
      result <- awaitProcess stdout
      return $ fn result

awaitShell :: String -> IO String
awaitShell command = do
  processInfo <- spawnShell command
  let stdout = onlyStdout processInfo
  awaitProcess $ Just stdout