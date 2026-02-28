module Dotf.Options (
  Command(..),
  PluginCommand(..),
  ProfileCommand(..),
  WatchlistCommand(..),

  readOptsOrTui,
  commandParser,
) where

import           Data.String.Interpolate (__i)
import           Data.Text               (Text, pack)
import qualified Data.Text               as T
import           Data.Version            (showVersion)
import           Options.Applicative
import           Paths_dotf              (version)

-----------
-- Types --
-----------

data Command
  -- Setup
  = Init Text (Maybe Text)
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
  | Save (Maybe Text)
  -- Maintenance
  | Consolidate Bool  -- ^ True = apply, False = dry-run
  -- Manual git
  | Stage [FilePath]
  | Unstage [FilePath]
  | Commit (Maybe Text)
  | Push
  | Pull
  | Status
  | Diff
  | GitRaw [String]
  | Packages Bool  -- ^ True = install, False = list only
  | Freeze FilePath
  | Unfreeze FilePath
  | Frozen
  | SuggestIgnore
  | SuggestAssign
  | Resolve
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
  <> command "consolidate" (info parseConsolidate (progDesc "Consolidate plugin paths"))
  <> command "save"      (info parseSave      (progDesc "Auto-stage, commit, push"))
  <> command "stage"     (info parseStage     (progDesc "Stage files"))
  <> command "unstage"   (info parseUnstage   (progDesc "Unstage files"))
  <> command "commit"    (info parseCommit    (progDesc "Commit staged changes"))
  <> command "push"      (info (pure Push)    (progDesc "Push to remote"))
  <> command "pull"      (info (pure Pull)    (progDesc "Pull from remote"))
  <> command "status"    (info (pure Status)  (progDesc "Show git status"))
  <> command "diff"      (info (pure Diff)    (progDesc "Show git diff"))
  <> command "git"       (info parseGitRaw    (progDesc "Raw git passthrough"))
  <> command "freeze"    (info parseFreeze    (progDesc "Freeze a tracked file (skip-worktree)"))
  <> command "unfreeze"  (info parseUnfreeze  (progDesc "Unfreeze a frozen file"))
  <> command "frozen"    (info (pure Frozen)  (progDesc "List all frozen files"))
  <> command "packages"  (info parsePackages  (progDesc "List/install OS packages for active plugins"))
  <> command "suggest-ignore" (info (pure SuggestIgnore) (progDesc "AI-assisted gitignore management"))
  <> command "suggest-assign" (info (pure SuggestAssign) (progDesc "AI-assisted file-to-plugin assignment"))
  <> command "resolve" (info (pure Resolve) (progDesc "AI-assisted merge conflict resolution"))
  )

parseInit :: Parser Command
parseInit = Init
  <$> argument (pack <$> str) (metavar "REPO_URL")
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
    splitCommas s = map T.strip $ T.splitOn "," (pack s)

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

parseFreeze :: Parser Command
parseFreeze = Freeze
  <$> argument str (metavar "FILE")

parseUnfreeze :: Parser Command
parseUnfreeze = Unfreeze
  <$> argument str (metavar "FILE")

parseConsolidate :: Parser Command
parseConsolidate = Consolidate
  <$> switch
      (  long "apply"
      <> help "Apply changes (default is dry-run)"
      )

parseSave :: Parser Command
parseSave = Save
  <$> optional (argument (pack <$> str) (metavar "MSG"))

parseStage :: Parser Command
parseStage = Stage
  <$> some (argument str (metavar "FILE..."))

parseUnstage :: Parser Command
parseUnstage = Unstage
  <$> some (argument str (metavar "FILE..."))

parseCommit :: Parser Command
parseCommit = Commit
  <$> optional (argument (pack <$> str) (metavar "MSG"))

parsePackages :: Parser Command
parsePackages = Packages
  <$> switch
      (  long "install"
      <> help "Install missing packages"
      )

parseGitRaw :: Parser Command
parseGitRaw = GitRaw
  <$> many (argument str (metavar "ARGS..."))

-------------
-- Methods --
-------------

-- | Exported parser for testing.
commandParser :: Parser Command
commandParser = parseCommand

-- | Parse command or return Nothing (launch TUI).
readOptsOrTui :: IO (Maybe Command)
readOptsOrTui = execParser $ info (optional parseCommand <**> helper <**> versionOpt)
  (  fullDesc
  <> progDesc [__i|dotf v#{showVersion version} - Modular dotfile manager|]
  )

versionOpt :: Parser (a -> a)
versionOpt = infoOption
  [__i|dotf v#{showVersion version}|]
  (  long "version"
  <> help "Show version"
  )
