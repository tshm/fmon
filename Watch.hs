{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
# ifdef DEBUG
{-# LANGUAGE StandaloneDeriving #-}
# endif
module Watch where
import System.Process (readProcessWithExitCode)
import Data.String (fromString)
import qualified System.FSNotify as FSN
# ifdef DEBUG
deriving instance Show FSN.WatchConfig
deriving instance Show FSN.Debounce
# endif

-- | fork and run a given command, and then Return stdout.
-- >>> take 2 `fmap` words `fmap` run ["ghc", "--version"]
-- ["The","Glorious"]
run :: [String] -> IO String
run (pname:args) = do
  (_, out, err) <- readProcessWithExitCode pname args ""
  return $ unlines [out, err]
run _ = return "No command specified."

-- | Watch current folder and trigger action.
-- | It will deactivate itself while running action.
watch :: FilePath -> Bool -> [String] -> (String -> IO ()) -> IO ()
watch dir recurse cmd log' = do
  mgr <- FSN.startManager
  _ <- watchFunc mgr (fromString dir) (const True) $ \evt -> do
    _ <- FSN.stopManager mgr
    log' $ show evt
    log' $ " > " ++ unwords cmd
# ifdef DEBUG
    log' $ show FSN.defaultConfig
# endif
    run cmd >>= log'
    watch dir recurse cmd log'
  return ()
    where watchFunc = if recurse then FSN.watchTree else FSN.watchDir

