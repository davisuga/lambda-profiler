import Control.Concurrent
import System.Environment (getArgs)

fibs = 1 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

fib :: Int -> Int
fib x = fibs !! x

incMVar :: (Num a, Show a) => MVar a -> a -> IO ()
incMVar var by = do
  value <- takeMVar var
  putMVar var (by + value)

storeFib :: (Num b, Enum b) => MVar Int -> Int -> b -> IO ()
storeFib storedFibs fibNum forks = do
  mapM_ (const $ forkIO $ incMVar storedFibs (fib fibNum)) [1 .. forks]
  takeMVar storedFibs >>= print
  putMVar storedFibs fibNum

main = do
  [fibNum, forks] <- getArgs
  let num = read fibNum
  mvar <- newMVar 0
  storeFib mvar num (read forks)

  let sas =
        [1 .. 20]
          <$ ( \n -> do
                 threadDelay (n * 100000000)
                 takeMVar mvar >>= print
                 return n
             )

  return sas