import Test.DocTest
import System.FilePath
import System.Directory

-- |
-- >>> collectTestFiles ["a.cpp","a.hs","c",".d"]
-- ["a.hs"]
--
collectTestFiles :: [FilePath] -> [FilePath]
collectTestFiles files = filter isHs files where
  isHs = (== ".hs") . snd . splitExtension

main :: IO ()
main = do
  setCurrentDirectory "src"
  files <- fmap collectTestFiles $ getDirectoryContents "."
  -- mapM_ print files
  doctest $ "-isrc" : files

