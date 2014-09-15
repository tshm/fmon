module OptionParser where
import qualified System.Console.GetOpt as O

data Options = Options
  { optHelp    :: Bool
  , optVersion :: Bool
  , optDir     :: FilePath
  , optRecurse :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optHelp    = False
  , optVersion = False
  , optDir     = "."
  , optRecurse = False
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
  , O.Option "r" ["recurse"]
      (O.NoArg (\opts -> opts { optRecurse = True }))
      "watch directory recursively"
  ]

-- | parse command line argument into list of options
-- >>> fmap fst $ parseOpts ["-v", "test"] "fmon"
-- Options {optHelp = False, optVersion = True, optDir = ".", optRecurse = False}
--
-- >>> fmap fst $ parseOpts ["-h", "test"] "fmon"
-- Options {optHelp = True, optVersion = False, optDir = ".", optRecurse = False}
--
-- >>> fmap (head . words . head . snd) $ parseOpts ["-h", "test"] "fmon"
-- "Usage:"
--
-- >>> :{
--   System.IO.Error.catchIOError
--     (parseOpts ["-x", "test"] "fmon")
--     (\_ -> return (defaultOptions, ["xx"]))
-- :}
-- (Options {optHelp = False, optVersion = False, optDir = ".", optRecurse = False},["xx"])
--
-- >>> parseOpts ["cmd", "test"] "fmon"
-- (Options {optHelp = False, optVersion = False, optDir = ".", optRecurse = False},["cmd","test"])
--
-- >>> parseOpts ["-d", "dir", "cmd"] "fmon"
-- (Options {optHelp = False, optVersion = False, optDir = "dir", optRecurse = False},["cmd"])
--
-- >>> parseOpts ["-r", "cmd"] "fmon"
-- (Options {optHelp = False, optVersion = False, optDir = ".", optRecurse = True},["cmd"])
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

