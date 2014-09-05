module OptionParser where
import qualified System.Console.GetOpt as O

data Options = Options
  { optHelp    :: Bool
  , optVersion :: Bool
  , optDir     :: FilePath
  , optShallow :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optHelp    = False
  , optVersion = False
  , optDir     = "."
  , optShallow = False
  }

options :: [O.OptDescr (Options -> Options)]
options =
  [ O.Option "h" ["help"]
      (O.NoArg (\opts -> opts { optHelp = True }))
      "show help"
  , O.Option "v" ["version"]
      (O.NoArg (\opts -> opts { optVersion = True }))
      "show version number"
  , O.Option "d" ["directory"]
      (O.ReqArg (\d opts -> opts { optDir = d }) "DIR")
      "directory to watch"
  , O.Option "s" ["shallow"]
      (O.NoArg (\opts -> opts { optShallow = True }))
      "do not watch recursively"
  ]

-- | parse command line argument into list of options
-- >>> fmap fst $ parseOpts ["-v", "test"] "fmon"
-- Options {optHelp = False, optVersion = True, optDir = ".", optShallow = False}
--
-- >>> fmap fst $ parseOpts ["-h", "test"] "fmon"
-- Options {optHelp = True, optVersion = False, optDir = ".", optShallow = False}
--
-- >>> fmap (head . words . head . snd) $ parseOpts ["-h", "test"] "fmon"
-- "Usage:"
--
-- >>> :{
--   System.IO.Error.catchIOError
--     (parseOpts ["-x", "test"] "fmon")
--     (\_ -> return (defaultOptions, ["xx"]))
-- :}
-- (Options {optHelp = False, optVersion = False, optDir = ".", optShallow = False},["xx"])
--
-- >>> parseOpts ["cmd", "test"] "fmon"
-- (Options {optHelp = False, optVersion = False, optDir = ".", optShallow = False},["cmd","test"])
--
-- >>> parseOpts ["-d", "dir", "cmd"] "fmon"
-- (Options {optHelp = False, optVersion = False, optDir = "dir", optShallow = False},["cmd"])
--
-- >>> parseOpts ["-s", "cmd"] "fmon"
-- (Options {optHelp = False, optVersion = False, optDir = ".", optShallow = True},["cmd"])
--
parseOpts :: [String] -> String -> IO (Options, [String])
parseOpts argv progname =
  case O.getOpt O.Permute options argv of
    (o,n,[]  ) -> if optHelp resultOptions 
                    then return (resultOptions, [usage])
                    else return (resultOptions, n)
               where
      resultOptions = foldl (flip id) defaultOptions o
    (_,_,errs) -> showUsage errs
    where
      usage = O.usageInfo header options
      showUsage errs = ioError (userError (concat errs ++ usage))
      header = "Usage: " ++ progname ++ " [OPTION...] command string..."

