module Dotf.Options (
  Command(..),
  PluginCommand(..),
  ProfileCommand(..),
  WatchlistCommand(..),

  readOpts,
  commandParser,
) where

import           Data.String.Interpolate (__i)
import           Data.Text               (Text, pack)
import           Data.Version            (showVersion)
import           Options.Applicative
import           Paths_dotf              (version)

-----------
-- Types --
-----------

data Command
  -- Setup
  = Init String (Maybe Text)
  | New
  | Migrate
  -- Plugins
  | PluginCmd PluginCommand
  -- Profiles
  | ProfileCmd ProfileCommand
  -- Tracking
  | Track FilePath (Maybe Text)
  | Untrack FilePath
  | Ignore Text
  -- Discovery
  | Untracked (Maybe Text)
  | WatchlistCmd WatchlistCommand
  -- Save
  | Save (Maybe String)
  -- Manual git
  | Stage [FilePath]
  | Unstage [FilePath]
  | Commit (Maybe String)
  | Push
  | Pull
  | Status
  | Diff
  | GitRaw [String]
  deriving Show

data PluginCommand
  = PluginList
  | PluginNew Text (Maybe Text)
  | PluginDelete Text
  | PluginInstall [Text]
  | PluginRemove [Text]
  | PluginInfo Text
  deriving Show

data ProfileCommand
  = ProfileList
  | ProfileNew Text [Text]
  | ProfileDelete Text
  | ProfileActivate Text
  | ProfileDeactivate
  | ProfileShow
  deriving Show

data WatchlistCommand
  = WatchlistAdd FilePath
  | WatchlistRemove FilePath
  | WatchlistList
  deriving Show

-------------
-- Parsers --
-------------

parseCommand :: Parser Command
parseCommand = hsubparser
  (  command "init"      (info parseInit      (progDesc "Clone bare repo + checkout"))
  <> command "new"       (info (pure New)     (progDesc "Create new bare repository"))
  <> command "migrate"   (info (pure Migrate) (progDesc "Scaffold v3 metadata on existing repo"))
  <> command "plugin"    (info parsePlugin    (progDesc "Plugin management"))
  <> command "profile"   (info parseProfile   (progDesc "Profile management"))
  <> command "track"     (info parseTrack     (progDesc "Track a file"))
  <> command "untrack"   (info parseUntrack   (progDesc "Untrack a file"))
  <> command "ignore"    (info parseIgnore    (progDesc "Add pattern to .gitignore"))
  <> command "untracked" (info parseUntracked (progDesc "List untracked files"))
  <> command "watchlist" (info parseWatchlist (progDesc "Manage watchlist"))
  <> command "save"      (info parseSave      (progDesc "Auto-stage, commit, push"))
  <> command "stage"     (info parseStage     (progDesc "Stage files"))
  <> command "unstage"   (info parseUnstage   (progDesc "Unstage files"))
  <> command "commit"    (info parseCommit    (progDesc "Commit staged changes"))
  <> command "push"      (info (pure Push)    (progDesc "Push to remote"))
  <> command "pull"      (info (pure Pull)    (progDesc "Pull from remote"))
  <> command "status"    (info (pure Status)  (progDesc "Show git status"))
  <> command "diff"      (info (pure Diff)    (progDesc "Show git diff"))
  <> command "git"       (info parseGitRaw    (progDesc "Raw git passthrough"))
  )

parseInit :: Parser Command
parseInit = Init
  <$> argument str (metavar "REPO_URL")
  <*> optional (option (pack <$> str)
      (  long "profile"
      <> short 'p'
      <> metavar "NAME"
      <> help "Profile to activate after clone"
      ))

parsePlugin :: Parser Command
parsePlugin = PluginCmd <$> hsubparser
  (  command "list"    (info (pure PluginList)       (progDesc "List all plugins"))
  <> command "new"     (info parsePluginNew          (progDesc "Create plugin"))
  <> command "delete"  (info parsePluginDelete       (progDesc "Delete plugin"))
  <> command "install" (info parsePluginInstall      (progDesc "Install plugin(s)"))
  <> command "remove"  (info parsePluginRemove       (progDesc "Remove plugin(s)"))
  <> command "info"    (info parsePluginInfo         (progDesc "Show plugin details"))
  )

parsePluginNew :: Parser PluginCommand
parsePluginNew = PluginNew
  <$> argument (pack <$> str) (metavar "NAME")
  <*> optional (option (pack <$> str)
      (  long "desc"
      <> metavar "TEXT"
      <> help "Plugin description"
      ))

parsePluginDelete :: Parser PluginCommand
parsePluginDelete = PluginDelete
  <$> argument (pack <$> str) (metavar "NAME")

parsePluginInstall :: Parser PluginCommand
parsePluginInstall = PluginInstall
  <$> some (argument (pack <$> str) (metavar "NAME..."))

parsePluginRemove :: Parser PluginCommand
parsePluginRemove = PluginRemove
  <$> some (argument (pack <$> str) (metavar "NAME..."))

parsePluginInfo :: Parser PluginCommand
parsePluginInfo = PluginInfo
  <$> argument (pack <$> str) (metavar "NAME")

parseProfile :: Parser Command
parseProfile = ProfileCmd <$> hsubparser
  (  command "list"       (info (pure ProfileList)       (progDesc "List profiles"))
  <> command "new"        (info parseProfileNew          (progDesc "Create profile"))
  <> command "delete"     (info parseProfileDelete       (progDesc "Delete profile"))
  <> command "activate"   (info parseProfileActivate     (progDesc "Activate profile"))
  <> command "deactivate" (info (pure ProfileDeactivate) (progDesc "Deactivate profile"))
  <> command "show"       (info (pure ProfileShow)       (progDesc "Show active profile"))
  )

parseProfileNew :: Parser ProfileCommand
parseProfileNew = ProfileNew
  <$> argument (pack <$> str) (metavar "NAME")
  <*> option (splitCommas <$> str)
      (  long "plugins"
      <> metavar "a,b,c"
      <> help "Comma-separated plugin list"
      )
  where
    splitCommas :: String -> [Text]
    splitCommas s = map (pack . trim) $ splitOn ',' s

    splitOn :: Char -> String -> [String]
    splitOn _ [] = [""]
    splitOn c (x:xs)
      | x == c    = "" : splitOn c xs
      | otherwise = case splitOn c xs of
          []    -> [x:[]]
          (h:t) -> (x:h) : t

    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

parseProfileDelete :: Parser ProfileCommand
parseProfileDelete = ProfileDelete
  <$> argument (pack <$> str) (metavar "NAME")

parseProfileActivate :: Parser ProfileCommand
parseProfileActivate = ProfileActivate
  <$> argument (pack <$> str) (metavar "NAME")

parseTrack :: Parser Command
parseTrack = Track
  <$> argument str (metavar "FILE")
  <*> optional (option (pack <$> str)
      (  long "plugin"
      <> short 'p'
      <> metavar "NAME"
      <> help "Plugin to assign file to"
      ))

parseUntrack :: Parser Command
parseUntrack = Untrack
  <$> argument str (metavar "FILE")

parseIgnore :: Parser Command
parseIgnore = Ignore . pack
  <$> argument str (metavar "PATTERN")

parseUntracked :: Parser Command
parseUntracked = Untracked
  <$> optional (option (pack <$> str)
      (  long "plugin"
      <> short 'p'
      <> metavar "NAME"
      <> help "Show only plugin-scoped untracked"
      ))

parseWatchlist :: Parser Command
parseWatchlist = WatchlistCmd <$> hsubparser
  (  command "add"    (info parseWatchlistAdd    (progDesc "Add path to watchlist"))
  <> command "remove" (info parseWatchlistRemove (progDesc "Remove path from watchlist"))
  <> command "list"   (info (pure WatchlistList) (progDesc "List watchlist paths"))
  )

parseWatchlistAdd :: Parser WatchlistCommand
parseWatchlistAdd = WatchlistAdd
  <$> argument str (metavar "PATH")

parseWatchlistRemove :: Parser WatchlistCommand
parseWatchlistRemove = WatchlistRemove
  <$> argument str (metavar "PATH")

parseSave :: Parser Command
parseSave = Save
  <$> optional (argument str (metavar "MSG"))

parseStage :: Parser Command
parseStage = Stage
  <$> some (argument str (metavar "FILE..."))

parseUnstage :: Parser Command
parseUnstage = Unstage
  <$> some (argument str (metavar "FILE..."))

parseCommit :: Parser Command
parseCommit = Commit
  <$> optional (argument str (metavar "MSG"))

parseGitRaw :: Parser Command
parseGitRaw = GitRaw
  <$> many (argument str (metavar "ARGS..."))

-------------
-- Methods --
-------------

-- | Exported parser for testing.
commandParser :: Parser Command
commandParser = parseCommand

readOpts :: IO Command
readOpts = execParser $ info (parseCommand <**> helper <**> versionOpt)
  (  fullDesc
  <> progDesc [__i|dotf v#{showVersion version} - Modular dotfile manager|]
  )

versionOpt :: Parser (a -> a)
versionOpt = infoOption
  [__i|dotf v#{showVersion version}|]
  (  long "version"
  <> help "Show version"
  )
