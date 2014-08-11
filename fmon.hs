{-# LANGUAGE CPP, OverloadedStrings #-}
import System.Environment (getArgs)
import System.Process (readProcess)
import qualified System.FSNotify as FSN -- (withManagerConf, watchTree, Event, defaultConfig)
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

print' :: Show a => a -> IO ()
putStrLn' :: String -> IO ()
# ifdef OS_Win32
-- for win32 environment, force buffer flush every method call.
print' x    = print x    >> hFlush stdout
putStrLn' x = putStrLn x >> hFlush stdout
# else
print' = print
putStrLn' = putStrLn
# endif

run :: [String] -> IO String
run (pname:args) = readProcess pname args ""
run _ = return "No command specified."

watch :: [String] -> IO ()
watch cmd = do
  mgr <- FSN.startManager
  _ <- FSN.watchTree mgr "." (const True) $ \evt -> do
    _ <- FSN.stopManager mgr
    print' evt
    putStrLn' $ unwords cmd
    run cmd >>= putStrLn'
    watch cmd
  return ()

main :: IO ()
main = do
  cmd <- getArgs
  if null cmd
    then putStrLn' "Please provide a command as a argument."
    else do
      putStrLn' $ "fmon starting... command = " ++ (unwords cmd)
      watch cmd
      forever $ threadDelay maxBound

