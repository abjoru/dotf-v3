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

  -- * Watchlist commands
  runWatchlistAdd,
  runWatchlistRemove,
  runWatchlistList,

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

import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Dotf.Config
import           Dotf.Git
import           Dotf.Path            (findMatchingPlugin, normalizePath)
import           Dotf.Plugin
import           Dotf.Profile
import           Dotf.State
import           Dotf.Templates
import           Dotf.Tracking
import           Dotf.Types
import           Dotf.Utils
import           System.Directory     (createDirectoryIfMissing,
                                       doesDirectoryExist)
import           System.Exit          (exitFailure)
import qualified System.Process.Typed as PT

-----------
-- Setup --
-----------

runInit :: GitEnv -> String -> Maybe Text -> IO ()
runInit env url mProfile = do
  putStrLn $ "Cloning " ++ url ++ "..."
  result <- gitCloneBare env url
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
      st <- loadLocalState env
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
    Right () -> putStrLn $ "Installed: " ++ T.unpack (T.intercalate ", " names)

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
      st <- loadLocalState env
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
      st <- loadLocalState env
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
    Right () -> putStrLn $ "Activated profile: " ++ T.unpack name

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
      st <- loadLocalState env
      case showActiveProfile prf st of
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
    when True  m = m
    when False _ = pure ()

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

------------------------------
-- Save / git commands       --
------------------------------

runSave :: GitEnv -> Maybe String -> IO ()
runSave env mMsg = do
  -- Auto-stage all modified tracked files
  stageResult <- gitTrackedUnstaged env
  case stageResult of
    Left err -> handleError err
    Right unstaged -> do
      mapM_ (gitStage env) unstaged
      -- Check if there's anything to commit
      staged <- gitTrackedStaged env
      case staged of
        Left err -> handleError err
        Right files
          | null files -> putStrLn "Nothing to save."
          | otherwise -> do
            let msg = maybe "dotf save" id mMsg
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

runCommit :: GitEnv -> Maybe String -> IO ()
runCommit env mMsg = do
  let msg = maybe "dotf commit" id mMsg
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
    Left err -> handleError err
    Right () -> putStrLn "Pulled."

runStatus :: GitEnv -> IO ()
runStatus env = do
  result <- gitStatus env
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
  cfg <- gitBare env args
  exitCode <- PT.runProcess cfg
  case exitCode of
    PT.ExitSuccess   -> pure ()
    PT.ExitFailure _ -> exitFailure

-----------
-- Utils --
-----------

handleError :: DotfError -> IO ()
handleError err = do
  putStrLn $ "Error: " ++ formatError err
  exitFailure

formatError :: DotfError -> String
formatError (GitError code msg)         = "git error (" ++ show code ++ "): " ++ T.unpack msg
formatError (ConfigError msg)           = "config: " ++ T.unpack msg
formatError (PathConflict a b path)     = "path conflict: " ++ path ++ " claimed by " ++ T.unpack a ++ " and " ++ T.unpack b
formatError (PluginNotFound name)       = "plugin not found: " ++ T.unpack name
formatError (ProfileNotFound name)      = "profile not found: " ++ T.unpack name
formatError (DependencyError msg)       = "dependency: " ++ T.unpack msg
formatError (UnassignedFilesExist _)    = "unassigned files exist"
formatError (ValidationError msg)       = T.unpack msg
