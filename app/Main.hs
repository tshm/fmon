{-# LANGUAGE CPP #-}
module Main where
import Watch
import OptionParser
import Paths_fmon (version)  -- read from cabal file.
import Data.Version (showVersion)
import System.Environment (getArgs, getProgName)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
# ifdef OS_Win32
import System.IO (hFlush, stdout)
# endif

log' :: String -> IO ()
# ifdef OS_Win32
-- for win32 environment, force buffer flush every method call.
log' x = putStrLn x >> hFlush stdout
# else
log' = putStrLn
# endif

main :: IO ()
main = do
  argv <- getArgs
  progname <- getProgName
  opts <- parseOpts argv progname
  case opts of
    (Options {optVersion = True}, _) -> log' $ "fmon " ++ showVersion version
    (Options {optHelp    = True}, usage) -> log' $ unwords usage
    (Options {optDir = dir, optRecurse = recurse}, cmd) -> do
      log' $ progname ++ " starting... command = " ++ (unwords cmd)
      watch dir recurse cmd log'
      forever $ threadDelay maxBound

