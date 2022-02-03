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
import Data.Tuple
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

getCommand :: Show a => Maybe a -> String
getCommand Nothing = ""
getCommand (Just pid) = "ps ax -o pid,rss  | grep " ++ show pid ++ "|awk -F\" \" '{print $2}'   | tee ps"


castTupleToArgs function args = function `uncurry` args
a |> b =
  case a of
    a -> b a
    _ -> b a
a /> b = b `uncurry` a

data Table = Table
  { cbo :: String,
    employees :: [String]
  }
data HomeToWorkDistance = HomeToWorkDistance
data ParamsSession = ParamsSession
data Coords = Coords
data CBODescription = CBODescription
data Branch = Branch

addHomeToWorkDistance [Branch] Coords  = HomeToWorkDistance
addCoordinates ParamsSession CBODescription = Coords
-- (|>) :: a -> (a -> b) -> b
addCBODescription :: [String] -> String -> CBODescription
addCBODescription _ _ = CBODescription

-- Sends an email
(📩) message title receiver = do
  print $ "Sending email to" ++ receiver ++ " with title " ++ title ++ " and message " ++ message

-- Let it ✨ shine ✨
(✨) content = "✨ " ++ content ++ " ✨"

test = do
  let table = Table {employees = ["John", "Peter"], cbo = "some-random-code"}
  let paramsSession = ParamsSession
  let branches = [Branch]

  let resultMeh = addHomeToWorkDistance  branches (addCoordinates  paramsSession (addCBODescription (employees table) $ cbo table))
  let tableCBO = cbo table
  let tableEmployees = employees table

  let resultNicest =
          (tableEmployees, tableCBO)
            /> addCBODescription
            |> addCoordinates paramsSession
            |> addHomeToWorkDistance branches

  let solutionA =
          addCBODescription (employees table) (cbo table)
            |> addCoordinates paramsSession
            |> addHomeToWorkDistance branches

  [[1,2],[2,3],[3,4]]
        |> flatten
        |> map (* 2)
        |> map (+ 5)
        |> sum

  -- let resultMeh =
  --       addHomeToWorkDistance
  --         ( addCoordinates
  --             (addCBODescription (employees table) $ cbo table)
  --             paramsSession
  --         )
  --         branches

flatten :: [[a]] -> [a]
flatten = foldl (++) []
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
    _ -> error "Unable to start process"

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