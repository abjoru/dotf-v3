module Dotf.Commands (
  -- * Setup
  runInit,
  runNew,
  runMigrate,
  migrate,
  scaffoldMetadata,

  -- * Plugin commands
  runPluginList,
  runPluginNew,
  runPluginDelete,
  runPluginInstall,
  runPluginRemove,
  runPluginInfo,

  -- * Profile commands
  runProfileList,
  runProfileNew,
  runProfileDelete,
  runProfileActivate,
  runProfileDeactivate,
  runProfileShow,

  -- * Tracking commands
  runTrack,
  runUntrack,
  runIgnore,
  runUntracked,

  -- * Freeze commands
  runFreeze,
  runUnfreeze,
  runFrozen,

  -- * Watchlist commands
  runWatchlistAdd,
  runWatchlistRemove,
  runWatchlistList,

  -- * Package commands
  runPackages,

  -- * AI-assisted commands
  runSuggestIgnore,
  runSuggestAssign,
  runResolveConflicts,

  -- * Maintenance commands
  runConsolidate,

  -- * Save / git commands
  runSave,
  runStage,
  runUnstage,
  runCommit,
  runPush,
  runPull,
  runStatus,
  runDiff,
  runGitRaw,
) where

import           Control.Monad        (unless, when)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe, mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Dotf.Config
import           Dotf.Git
import           Dotf.Packages
import           Dotf.Path            (consolidatePluginPaths,
                                       findMatchingPlugin, isSubpathOf,
                                       normalizePath)
import           Dotf.Plugin
import           Dotf.Profile
import           Dotf.State
import           Dotf.Templates
import           Dotf.Tracking
import           Dotf.Types
import           Dotf.Utils
import           System.Directory     (createDirectoryIfMissing,
                                       doesDirectoryExist, doesFileExist)
import           System.Exit          (exitFailure)
import           System.FilePath      ((</>))
import           System.IO            (hFlush, hPutStrLn, stderr, stdout)
import           System.Process       (callProcess)
import qualified System.Process.Typed as PT

-----------
-- Setup --
-----------

runInit :: GitEnv -> Text -> Maybe Text -> IO ()
runInit env url mProfile = do
  putStrLn $ "Cloning " ++ T.unpack url ++ "..."
  result <- gitCloneBare env (T.unpack url)
  case result of
    Left err -> handleError err
    Right () -> do
      putStrLn "Clone complete."
      case mProfile of
        Nothing -> putStrLn "No profile activated. Use 'dotf profile activate <name>'."
        Just name -> do
          putStrLn $ "Activating profile " ++ T.unpack name ++ "..."
          runProfileActivate env name

runNew :: GitEnv -> IO ()
runNew env = do
  exists <- doesDirectoryExist (dotfGitDir env)
  if exists
    then putStrLn "Error: Bare repo already exists at ~/.dotf" >> exitFailure
    else do
      result <- gitInitBare env
      case result of
        Left err -> handleError err
        Right () -> do
          scaffoldMetadata env
          putStrLn "New bare repository created at ~/.dotf"
          putStrLn "Metadata scaffolded at ~/.config/dotf/"

runMigrate :: GitEnv -> IO ()
runMigrate env = do
  result <- migrate env
  case result of
    Left err -> handleError err
    Right () -> putStrLn migrateSuccessMessage

-- | Core migration logic: scaffold metadata and git-add config files.
-- Returns Left on missing repo instead of calling exitFailure.
migrate :: GitEnv -> IO (Either DotfError ())
migrate env = do
  exists <- doesDirectoryExist (dotfGitDir env)
  if not exists
    then pure $ Left (ConfigError "Missing bare repository at ~/.dotf")
    else do
      scaffoldMetadata env
      _ <- gitAdd env ".config/dotf/plugins.yaml"
      _ <- gitAdd env ".config/dotf/profiles.yaml"
      pure $ Right ()

scaffoldMetadata :: GitEnv -> IO ()
scaffoldMetadata env = do
  createDirectoryIfMissing True (metadataDir env)
  writeFile (pluginsFile env) scaffoldPluginsYaml
  writeFile (profilesFile env) scaffoldProfilesYaml

--------------------
-- Plugin commands --
--------------------

runPluginList :: GitEnv -> IO ()
runPluginList env = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> handleError err
    Right cfg -> do
      stResult <- loadLocalState env
      case stResult of
        Left err -> handleError err
        Right st -> do
          let items = listPlugins cfg st
          if null items
            then putStrLn "No plugins defined."
            else mapM_ printPlugin items
  where
    printPlugin (p, installed) = do
      let marker = if installed then "●" else "○" :: String
      putStrLn $ "  " ++ marker ++ " " ++ T.unpack (_pluginName p)
        ++ maybe "" (\d -> " - " ++ T.unpack d) (_pluginDescription p)

runPluginNew :: GitEnv -> Text -> Maybe Text -> IO ()
runPluginNew env name desc = do
  result <- createPlugin env name desc
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Created plugin: " ++ T.unpack name

runPluginDelete :: GitEnv -> Text -> IO ()
runPluginDelete env name = do
  result <- deletePlugin env name
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Deleted plugin: " ++ T.unpack name

runPluginInstall :: GitEnv -> [Text] -> IO ()
runPluginInstall env names = do
  result <- installPlugins env names
  case result of
    Left err -> handleError err
    Right () -> do
      putStrLn $ "Installed: " ++ T.unpack (T.intercalate ", " names)
      offerPackageInstall env names

runPluginRemove :: GitEnv -> [Text] -> IO ()
runPluginRemove env names = do
  result <- removePlugins env names
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Removed: " ++ T.unpack (T.intercalate ", " names)

runPluginInfo :: GitEnv -> Text -> IO ()
runPluginInfo env name = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> handleError err
    Right cfg -> do
      stResult <- loadLocalState env
      case stResult of
        Left err -> handleError err
        Right st -> do
          result <- pluginInfo env name cfg st
          case result of
            Left err -> handleError err
            Right (p, installed, files) -> do
              putStrLn $ "Plugin: " ++ T.unpack (_pluginName p)
              putStrLn $ "Status: " ++ if installed then "installed" else "not installed"
              maybe (pure ()) (\d -> putStrLn $ "Description: " ++ T.unpack d) (_pluginDescription p)
              case _pluginDepends p of
                [] -> pure ()
                ds -> putStrLn $ "Depends: " ++ T.unpack (T.intercalate ", " ds)
              putStrLn $ "Paths: " ++ show (_pluginPaths p)
              putStrLn $ "Files (" ++ show (length files) ++ "):"
              mapM_ (\f -> putStrLn $ "  " ++ f) files

---------------------
-- Profile commands --
---------------------

runProfileList :: GitEnv -> IO ()
runProfileList env = do
  prfResult <- loadProfileConfig env
  case prfResult of
    Left err -> handleError err
    Right prf -> do
      stResult <- loadLocalState env
      case stResult of
        Left err -> handleError err
        Right st -> do
          let items = listProfiles prf st
          if null items
            then putStrLn "No profiles defined."
            else mapM_ printProfile items
  where
    printProfile (p, active) = do
      let marker = if active then "★" else "○" :: String
      putStrLn $ "  " ++ marker ++ " " ++ T.unpack (_profileName p)

runProfileNew :: GitEnv -> Text -> [Text] -> IO ()
runProfileNew env name plugins = do
  result <- createProfile env name plugins
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Created profile: " ++ T.unpack name

runProfileDelete :: GitEnv -> Text -> IO ()
runProfileDelete env name = do
  result <- deleteProfile env name
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Deleted profile: " ++ T.unpack name

runProfileActivate :: GitEnv -> Text -> IO ()
runProfileActivate env name = do
  result <- activateProfile env name
  case result of
    Left (UnassignedFilesExist files) -> do
      putStrLn "Cannot activate: unassigned files remain:"
      mapM_ (\f -> putStrLn $ "  " ++ f) (take 20 files)
      let remaining = length files - 20
      if remaining > 0
        then putStrLn $ "  ... and " ++ show remaining ++ " more"
        else pure ()
      exitFailure
    Left err -> handleError err
    Right resolved -> do
      putStrLn $ "Activated profile: " ++ T.unpack name
      offerPackageInstall env resolved

runProfileDeactivate :: GitEnv -> IO ()
runProfileDeactivate env = do
  result <- deactivateProfile env
  case result of
    Left err -> handleError err
    Right () -> putStrLn "Profile deactivated. Full checkout restored."

runProfileShow :: GitEnv -> IO ()
runProfileShow env = do
  prfResult <- loadProfileConfig env
  case prfResult of
    Left err -> handleError err
    Right prf -> do
      stResult <- loadLocalState env
      case stResult of
        Left err -> handleError err
        Right st -> case showActiveProfile prf st of
          Nothing -> putStrLn "No active profile."
          Just (p, installed) -> do
            putStrLn $ "Active profile: " ++ T.unpack (_profileName p)
            putStrLn "Installed plugins:"
            mapM_ (\n -> putStrLn $ "  ● " ++ T.unpack n) installed

-----------------------
-- Tracking commands --
-----------------------

runTrack :: GitEnv -> FilePath -> Maybe Text -> IO ()
runTrack env path mPlugin = do
  -- Auto-detect plugin if not specified
  plugin <- case mPlugin of
    Just p  -> pure $ Just p
    Nothing -> do
      cfgResult <- loadPluginConfig env
      case cfgResult of
        Left _    -> pure Nothing
        Right cfg -> do
          let relPath = normalizePath (_geHome env) path
          pure $ findMatchingPlugin relPath (_pcPlugins cfg)
  result <- trackFile env path plugin
  case result of
    Left err -> handleError err
    Right () -> do
      let relPath = normalizePath (_geHome env) path
      putStr $ "Tracked: " ++ relPath
      case plugin of
        Just p  -> putStrLn $ " → " ++ T.unpack p
        Nothing -> putStrLn " (unassigned)"

runUntrack :: GitEnv -> FilePath -> IO ()
runUntrack env path = do
  result <- untrackFile env path
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Untracked: " ++ normalizePath (_geHome env) path

runIgnore :: GitEnv -> Text -> IO ()
runIgnore env pattern' = do
  result <- ignorePattern env pattern'
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Added to .gitignore: " ++ T.unpack pattern'

runUntracked :: GitEnv -> Maybe Text -> IO ()
runUntracked env mPlugin = case mPlugin of
  Just name -> do
    result <- discoverPluginUntracked env name
    case result of
      Left err -> handleError err
      Right files
        | null files -> putStrLn "No untracked files."
        | otherwise  -> mapM_ putStrLn files
  Nothing -> do
    result <- discoverUntracked env
    case result of
      Left err -> handleError err
      Right report -> do
        -- Print plugin-scoped
        mapM_ printPluginGroup (Map.toList $ _urPluginScoped report)
        -- Print watchlist
        case _urWatchlist report of
          [] -> pure ()
          wl -> do
            putStrLn "Watchlist:"
            mapM_ (\f -> putStrLn $ "  " ++ f) wl
        when (Map.null (_urPluginScoped report) && null (_urWatchlist report)) $
          putStrLn "No untracked files discovered."
  where
    printPluginGroup (name, files) = do
      putStrLn $ T.unpack name ++ ":"
      mapM_ (\f -> putStrLn $ "  " ++ f) files

---------------------
-- Freeze commands --
---------------------

runFreeze :: GitEnv -> FilePath -> IO ()
runFreeze env path = do
  result <- freezeFile env path
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Frozen: " ++ normalizePath (_geHome env) path

runUnfreeze :: GitEnv -> FilePath -> IO ()
runUnfreeze env path = do
  result <- unfreezeFile env path
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Unfrozen: " ++ normalizePath (_geHome env) path

runFrozen :: GitEnv -> IO ()
runFrozen env = do
  result <- listFrozen env
  case result of
    Left err -> handleError err
    Right fps
      | null fps  -> putStrLn "No frozen files."
      | otherwise -> mapM_ putStrLn fps

-----------------------
-- Watchlist commands --
-----------------------

runWatchlistAdd :: GitEnv -> FilePath -> IO ()
runWatchlistAdd env path = do
  result <- addWatchPath env path
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Added to watchlist: " ++ path

runWatchlistRemove :: GitEnv -> FilePath -> IO ()
runWatchlistRemove env path = do
  result <- removeWatchPath env path
  case result of
    Left err -> handleError err
    Right () -> putStrLn $ "Removed from watchlist: " ++ path

runWatchlistList :: GitEnv -> IO ()
runWatchlistList env = do
  result <- listWatchPaths env
  case result of
    Left err -> handleError err
    Right paths
      | null paths -> putStrLn "Watchlist is empty."
      | otherwise  -> mapM_ putStrLn paths

--------------------------
-- Maintenance commands --
--------------------------

runConsolidate :: GitEnv -> Bool -> IO ()
runConsolidate env apply = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> handleError err
    Right cfg -> do
      let changes = consolidatePluginPaths (_pcPlugins cfg) (_wlPaths (_pcWatchlist cfg))
      if null changes
        then putStrLn "Nothing to consolidate."
        else if apply
          then do
            let cfg' = foldr applyChange cfg changes
            savePluginConfig env cfg'
            putStrLn "Applied:"
            mapM_ printChange changes
          else do
            putStrLn "Proposed changes (use --apply to write):"
            mapM_ printChange changes
  where
    applyChange (name, _old, new') c =
      let ps = _pcPlugins c
      in case Map.lookup name ps of
           Nothing -> c
           Just p  -> c { _pcPlugins = Map.insert name (p { _pluginPaths = new' }) ps }
    printChange (name, old, new') = do
      putStrLn $ T.unpack name ++ ":"
      let groups = consolidatedGroups old new'
      if null groups
        then putStrLn "  (dedup only)"
        else mapM_ printGroup groups
      putStrLn ""
    printGroup (sources, target) = do
      let w = maximum (map length sources) + 2
      mapM_ (\s -> putStrLn $ "  " ++ s ++ replicate (max 1 (w - length s)) ' ' ++ "]") sources
      putStrLn $ replicate (w + 2) ' ' ++ "] -> " ++ target
    consolidatedGroups old new' =
      [ (srcs, t)
      | t <- new'
      , t `notElem` old
      , let srcs = filter (\o -> t `isSubpathOf` o) old
      , length srcs >= 2
      ]

------------------------------
-- Save / git commands       --
------------------------------

runSave :: GitEnv -> Maybe Text -> IO ()
runSave env mMsg = do
  -- Auto-stage all modified tracked files
  stageResult <- gitTrackedUnstaged env
  case stageResult of
    Left err -> handleError err
    Right unstaged -> do
      stageResults <- mapM (gitStage env) unstaged
      case sequence stageResults of
        Left err -> handleError err
        Right _  -> do
          -- Check if there's anything to commit
          staged <- gitTrackedStaged env
          case staged of
            Left err -> handleError err
            Right files
              | null files -> putStrLn "Nothing to save."
              | otherwise  -> do
                  let msg = T.unpack $ fromMaybe "dotf save" mMsg
                  commitResult <- gitCommit env msg
                  case commitResult of
                    Left err -> handleError err
                    Right () -> do
                      pushResult <- gitPush env
                      case pushResult of
                        Left err -> do
                          putStrLn "Committed but push failed:"
                          handleError err
                        Right () ->
                          putStrLn $ "Saved: " ++ show (length files) ++ " file(s)"

runStage :: GitEnv -> [FilePath] -> IO ()
runStage env files = do
  results <- mapM (gitStage env) files
  case sequence results of
    Left err -> handleError err
    Right _  -> putStrLn $ "Staged " ++ show (length files) ++ " file(s)"

runUnstage :: GitEnv -> [FilePath] -> IO ()
runUnstage env files = do
  results <- mapM (gitUnstage env) files
  case sequence results of
    Left err -> handleError err
    Right _  -> putStrLn $ "Unstaged " ++ show (length files) ++ " file(s)"

runCommit :: GitEnv -> Maybe Text -> IO ()
runCommit env mMsg = do
  let msg = T.unpack $ fromMaybe "dotf commit" mMsg
  result <- gitCommit env msg
  case result of
    Left err -> handleError err
    Right () -> putStrLn "Committed."

runPush :: GitEnv -> IO ()
runPush env = do
  result <- gitPush env
  case result of
    Left err -> handleError err
    Right () -> putStrLn "Pushed."

runPull :: GitEnv -> IO ()
runPull env = do
  result <- gitPull env
  case result of
    Right () -> putStrLn "Pulled."
    Left err -> do
      merging <- hasMergeHead env
      if merging
        then do
          putStrLn "Pull resulted in merge conflicts."
          runResolveConflicts env
        else handleError err

runStatus :: GitEnv -> IO ()
runStatus env = do
  pcfgE <- loadPluginConfig env
  pcfg <- case pcfgE of
    Left err -> do
      hPutStrLn stderr $ "Warning: " ++ displayError err ++ " (using empty config)"
      pure defaultPluginConfig
    Right cfg -> pure cfg
  let paths = scopePaths pcfg
  result <- gitStatus env paths
  case result of
    Left err -> handleError err
    Right s  -> putStr s

runDiff :: GitEnv -> IO ()
runDiff env = do
  result <- gitDiff env
  case result of
    Left err -> handleError err
    Right s  -> putStr s

runGitRaw :: GitEnv -> [String] -> IO ()
runGitRaw env args = do
  let cfg = gitBare env args
  exitCode <- PT.runProcess cfg
  case exitCode of
    PT.ExitSuccess   -> pure ()
    PT.ExitFailure _ -> exitFailure

-----------------------
-- Package commands  --
-----------------------

runPackages :: GitEnv -> Bool -> IO ()
runPackages env install = do
  dist <- detectDistro
  case dist of
    UnsupportedDistro -> putStrLn "Unsupported distro for package management."
    _ -> do
      cfgResult <- loadPluginConfig env
      case cfgResult of
        Left err -> handleError err
        Right cfg -> do
          stResult <- loadLocalState env
          case stResult of
            Left err -> handleError err
            Right st -> do
              let names = _lsInstalledPlugins st
                  plugins = mapMaybe (\n -> Map.lookup n (_pcPlugins cfg)) names
                  allPkgs = collectPackages dist plugins
                  caskPkgs = collectCaskPackages plugins
              installed <- listInstalledPackages dist
              let missing = filterUninstalled installed allPkgs
              let present = filter (`elem` installed) allPkgs
              if null allPkgs
                then putStrLn "No packages defined for active plugins."
                else do
                  unless (null present) $ do
                    putStrLn $ "Installed (" ++ show (length present) ++ "):"
                    mapM_ (\p -> putStrLn $ "  " ++ T.unpack p) present
                  unless (null missing) $ do
                    unless (null present) $ putStrLn ""
                    putStrLn $ "Missing (" ++ show (length missing) ++ "):"
                    mapM_ (\p -> putStrLn $ "  " ++ T.unpack p) missing
                  if install && not (null missing)
                    then installPackagesCli dist
                           (filter (`notElem` caskPkgs) missing)
                           (filter (`elem` caskPkgs) missing)
                    else unless (null missing) $
                           putStrLn "\nUse --install to install missing packages."

--------------------------
-- AI-assisted commands --
--------------------------

runSuggestIgnore :: GitEnv -> IO ()
runSuggestIgnore env = do
  hasClaude <- which "claude"
  unless hasClaude $ do
    putStrLn "Error: 'claude' CLI not found on PATH."
    putStrLn "Install: curl -fsSL https://claude.ai/install.sh | bash"
    dist <- detectDistro
    case dist of
      Arch -> putStrLn "    or:  paru -S claude-code"
      Osx  -> putStrLn "    or:  brew install --cask claude-code"
      _    -> pure ()
    exitFailure
  let home = _geHome env
  pcfgE <- loadPluginConfig env
  pcfg <- case pcfgE of
    Left err -> do
      hPutStrLn stderr $ "Warning: " ++ displayError err ++ " (using empty config)"
      pure defaultPluginConfig
    Right cfg -> pure cfg
  let scope = scopePaths pcfg
  trackedE   <- gitTracked env
  untrackedE <- gitUntracked env scope
  let tracked'   = either (const []) id trackedE
      untracked' = either (const []) id untrackedE
  ignoreExists <- doesFileExist (gitIgnoreFile env)
  ignoreContent <- if ignoreExists
    then T.unpack <$> TIO.readFile (gitIgnoreFile env)
    else pure ""
  let context = unlines
        [ "You are managing dotfiles in a git bare repo (~/.dotf/)."
        , "HOME: " ++ home
        , ""
        , "Current .gitignore:"
        , "```"
        , ignoreContent
        , "```"
        , ""
        , "Untracked files (scoped to managed paths):"
        , unlines (map ("  " ++) untracked')
        , ""
        , "Tracked files:"
        , unlines (map ("  " ++) tracked')
        , ""
        , "Present suggestions using AskUserQuestion with multiSelect so the"
        , "user can pick which patterns to apply. Group patterns by category"
        , "(e.g. caches, build artifacts, editor files). After the user"
        , "selects patterns, use Edit to append them to " ++ gitIgnoreFile env ++ "."
        , ""
        , "After adding new patterns, do a cleanup pass on the full .gitignore:"
        , "- Merge redundant entries (e.g. foo/ + foo/bar → foo/)"
        , "- Replace clusters of similar files with glob patterns"
        , "- Remove patterns that are subsumed by broader ones"
        , "- Organize into logical sections with comments"
        , "Present proposed cleanups via AskUserQuestion before applying."
        ]
      initialPrompt = "Analyze the untracked files and suggest "
        ++ ".gitignore patterns. Use AskUserQuestion to let me pick "
        ++ "which ones to apply. Then do a cleanup pass on the full "
        ++ ".gitignore to simplify and organize it."
  callProcess "claude"
    [ "--allowedTools", "Bash Read Edit Glob Grep AskUserQuestion"
    , "--add-dir", home
    , "--append-system-prompt", context
    , initialPrompt
    ]

runSuggestAssign :: GitEnv -> IO ()
runSuggestAssign env = do
  hasClaude <- which "claude"
  unless hasClaude $ do
    putStrLn "Error: 'claude' CLI not found on PATH."
    putStrLn "Install: curl -fsSL https://claude.ai/install.sh | bash"
    dist <- detectDistro
    case dist of
      Arch -> putStrLn "    or:  paru -S claude-code"
      Osx  -> putStrLn "    or:  brew install --cask claude-code"
      _    -> pure ()
    exitFailure
  let home = _geHome env
  pcfgE <- loadPluginConfig env
  pcfg <- case pcfgE of
    Left err -> do
      hPutStrLn stderr $ "Warning: " ++ displayError err ++ " (using empty config)"
      pure defaultPluginConfig
    Right cfg -> pure cfg
  let scope = scopePaths pcfg
  trackedE   <- gitTracked env
  untrackedE <- gitUntracked env scope
  let tracked'   = either (const []) id trackedE
      untracked' = either (const []) id untrackedE
      (_assigned, unassigned) = checkCoverage tracked' (_pcPlugins pcfg)
  if null unassigned && null untracked'
    then putStrLn "No unassigned or untracked files."
    else do
      let pluginDefs = unlines
            [ "  " ++ T.unpack name ++ ":"
              ++ maybe "" (\d -> " " ++ T.unpack d) (_pluginDescription p)
              ++ "\n    paths: " ++ show (_pluginPaths p)
            | (name, p) <- Map.toList (_pcPlugins pcfg)
            ]
          unassignedSection = if null unassigned then ""
            else "Unassigned tracked files (in git but no plugin):\n"
              ++ unlines (map ("  " ++) unassigned) ++ "\n"
          untrackedSection = if null untracked' then ""
            else "Untracked files (not in git yet, scoped to managed paths):\n"
              ++ unlines (map ("  " ++) untracked') ++ "\n"
          context = unlines
            [ "You are managing dotfiles in a git bare repo (~/.dotf/)."
            , "HOME: " ++ home
            , ""
            , "Plugin definitions:"
            , pluginDefs
            , ""
            , unassignedSection ++ untrackedSection
            , "For unassigned files: suggest which plugin they belong to."
            , "For untracked files: suggest whether to track + assign to a plugin."
            , "Present suggestions using AskUserQuestion with multiSelect, grouped"
            , "by target plugin. After the user selects, apply via Bash:"
            , "  dotf track <file> --plugin <name>"
            , "This command both tracks (if needed) and assigns in one step."
            ]
          initialPrompt = "Analyze the files and suggest plugin assignments. "
            ++ "Use AskUserQuestion to let me pick which ones to apply, "
            ++ "then run `dotf track --plugin` for each."
      callProcess "claude"
        [ "--allowedTools", "Bash Read Glob Grep AskUserQuestion"
        , "--add-dir", home
        , "--append-system-prompt", context
        , initialPrompt
        ]

runResolveConflicts :: GitEnv -> IO ()
runResolveConflicts env = do
  merging <- hasMergeHead env
  if not merging
    then putStrLn "No merge in progress."
    else do
      conflictsE <- gitConflictFiles env
      case conflictsE of
        Left err -> handleError err
        Right conflicts
          | null conflicts -> putStrLn "No conflicted files found."
          | otherwise -> do
            hasClaude <- which "claude"
            if hasClaude
              then launchConflictResolver env conflicts
              else do
                putStrLn "Conflicted files:"
                mapM_ (\f -> putStrLn $ "  " ++ f) conflicts
                putStrLn ""
                putStrLn "Resolve manually, then run:"
                putStrLn "  dotf git add <file>    # mark resolved"
                putStrLn "  dotf git commit        # complete merge"

launchConflictResolver :: GitEnv -> [FilePath] -> IO ()
launchConflictResolver env conflicts = do
  let home = _geHome env
  fileContents <- mapM (\f -> do
    let fullPath = home </> f
    content <- T.unpack <$> TIO.readFile fullPath
    pure $ "=== " ++ f ++ " ===\n" ++ content
    ) conflicts
  let context = unlines
        [ "You are resolving merge conflicts in a git bare repo (~/.dotf/)."
        , "HOME: " ++ home
        , ""
        , "Conflicted files:"
        , unlines fileContents
        , ""
        , "For each file, analyze the <<<<<<< / ======= / >>>>>>> conflict markers."
        , "Present your proposed resolution for each file using AskUserQuestion."
        , "After user approval, use Edit to resolve the conflict markers."
        , "Then mark resolved via Bash: dotf git add <file>"
        , "After all files are resolved, offer to complete the merge via:"
        , "  Bash: dotf git commit"
        ]
      initialPrompt = "Analyze the merge conflicts and present resolution "
        ++ "options. Use AskUserQuestion for each file, apply via Edit, "
        ++ "then mark resolved with `dotf git add`."
  callProcess "claude"
    [ "--allowedTools", "Bash Read Edit Glob Grep AskUserQuestion"
    , "--add-dir", home
    , "--append-system-prompt", context
    , initialPrompt
    ]

-- | Offer to install missing OS packages after plugin/profile activation.
offerPackageInstall :: GitEnv -> [PluginName] -> IO ()
offerPackageInstall env names = do
  dist <- detectDistro
  case dist of
    UnsupportedDistro -> pure ()
    _ -> do
      cfgResult <- loadPluginConfig env
      case cfgResult of
        Left _ -> pure ()
        Right cfg -> do
          let plugins = mapMaybe (\n -> Map.lookup n (_pcPlugins cfg)) names
              allPkgs = collectPackages dist plugins
              caskPkgs = collectCaskPackages plugins
          installed <- listInstalledPackages dist
          let missing = filterUninstalled installed allPkgs
          unless (null missing) $ do
            putStrLn "Uninstalled packages:"
            mapM_ (\p -> putStrLn $ "  " ++ T.unpack p) missing
            putStr "Install? [y/N] "
            hFlush stdout
            answer <- getLine
            when (answer `elem` ["y", "Y"]) $
              installPackagesCli dist
                (filter (`notElem` caskPkgs) missing)
                (filter (`elem` caskPkgs) missing)

-----------
-- Utils --
-----------

handleError :: DotfError -> IO ()
handleError err = do
  putStrLn $ "Error: " ++ displayError err
  exitFailure
