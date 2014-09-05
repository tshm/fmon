{-# LANGUAGE OverloadedStrings #-}
module Watch where
import System.Process (readProcessWithExitCode)
import qualified System.FSNotify as FSN

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
watch :: [String] -> (String -> IO ()) -> IO ()
watch cmd log' = do
  mgr <- FSN.startManager
  _ <- FSN.watchTree mgr "." (const True) $ \evt -> do
    _ <- FSN.stopManager mgr
    log' $ show evt
    log' $ " > " ++ unwords cmd
    run cmd >>= log'
    watch cmd log'
  return ()

