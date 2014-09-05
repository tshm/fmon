{-# LANGUAGE OverloadedStrings #-}
module Watch where
import System.Process (readProcessWithExitCode)
import Data.String (fromString)
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
watch :: FilePath -> Bool -> [String] -> (String -> IO ()) -> IO ()
watch dir shallow cmd log' = do
  mgr <- FSN.startManager
  _ <- watchFunc mgr (fromString dir) (const True) $ \evt -> do
    _ <- FSN.stopManager mgr
    log' $ show evt
    log' $ " > " ++ unwords cmd
    run cmd >>= log'
    watch dir shallow cmd log'
  return ()
    where watchFunc = if shallow then FSN.watchDir else FSN.watchTree

