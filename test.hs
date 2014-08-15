import Test.DocTest
import System.FilePath
import System.Directory

excludeFiles :: [FilePath]
excludeFiles = ["fmon.hs", "Test.hs", "Setup.hs"]

-- |
-- >>> collectTestFiles ["a.cpp","a.hs","c",".d","fmon.hs"]
-- ["a.hs"]
collectTestFiles :: [FilePath] -> [FilePath]
collectTestFiles files = filter (\n -> isHs n && inc n) files where
  isHs = (== ".hs") . snd . splitExtension
  inc n = not $ elem n excludeFiles

main :: IO ()
main = do
  files <- fmap collectTestFiles $ getDirectoryContents "."
  doctest $ "-isrc" : files

