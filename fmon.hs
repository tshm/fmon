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

handleEvent :: [String] -> FSN.Event -> IO ()
handleEvent cmd evt = do
  print' evt
  run cmd >>= putStrLn'

watchConfig :: FSN.WatchConfig
watchConfig = FSN.WatchConfig (FSN.Debounce (10^(12::Int))) 1000 False

main :: IO ()
main = FSN.withManagerConf watchConfig $ \mgr -> do
  cmd <- getArgs
  _ <- FSN.watchTree mgr "." (const True) (handleEvent cmd)

  if null cmd
    then putStrLn' "please provide command string."
    else (putStrLn' $ "fmon starting... command = " ++ (unwords cmd))
      >> (forever $ threadDelay maxBound)

