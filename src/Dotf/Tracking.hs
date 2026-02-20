module Dotf.Tracking (
  -- * Pure
  classifyUntracked,

  -- * IO
  trackFile,
  reassignFiles,
  untrackFile,
  ignorePattern,
  discoverUntracked,
  discoverPluginUntracked,
  addWatchPath,
  removeWatchPath,
  listWatchPaths,
) where

import           Control.Exception (IOException, try)
import           Control.Monad     (foldM)
import qualified Data.Map.Strict   as Map
import qualified Data.Set          as Set
import qualified Data.Text         as T
import           Dotf.Config
import           Dotf.Git
import           Dotf.Path         (isSubpathOf, normalizePath)
import           Dotf.State
import           Dotf.Types
import           Dotf.Utils        (appendToFile, gitIgnoreFile)

-- | Classify untracked files into plugin-scoped and watchlist buckets.
-- Files under a plugin's paths go to plugin bucket, remainder to watchlist
-- (if under a watchlist path).
classifyUntracked :: [RelPath]
                  -> Map.Map PluginName Plugin
                  -> [RelPath]  -- ^ Watchlist paths
                  -> (Map.Map PluginName [RelPath], [RelPath])
classifyUntracked files plugins watchPaths =
  foldr classify (Map.empty, []) files
  where
    classify f (plugMap, wl) =
      case findPluginMatch f of
        Just pname -> (Map.insertWith (++) pname [f] plugMap, wl)
        Nothing
          | any (\wp -> wp `isSubpathOf` f) watchPaths -> (plugMap, f:wl)
          | otherwise -> (plugMap, wl)

    findPluginMatch f =
      let matches = [ name
                     | (name, plugin) <- Map.toList plugins
                     , any (\pp -> pp `isSubpathOf` f) (_pluginPaths plugin)
                     ]
      in case matches of
        (m:_) -> Just m
        []    -> Nothing

-- | Track a file: git add + optionally assign to plugin with conflict resolution.
trackFile :: GitEnv -> RelPath -> Maybe PluginName -> IO (Either DotfError ())
trackFile env path mPlugin = do
  let relPath = normalizePath (_geHome env) path
  addResult <- gitAdd env relPath
  case addResult of
    Left err -> pure $ Left err
    Right () -> case mPlugin of
      Nothing       -> pure $ Right ()
      Just plugName -> reassignFiles env [relPath] plugName

-- | Assign files to a plugin, resolving cross-plugin path conflicts.
-- For each other plugin, directory-level paths that overlap with the reassigned
-- files are expanded to individual tracked files (minus the reassigned ones).
reassignFiles :: GitEnv -> [RelPath] -> PluginName -> IO (Either DotfError ())
reassignFiles env paths plugName = do
  let relPaths = map (normalizePath (_geHome env)) paths
  -- Git add each file
  addResult <- foldM (\acc fp -> case acc of
    Left err -> pure $ Left err
    Right () -> gitAdd env fp
    ) (Right ()) relPaths
  case addResult of
    Left err -> pure $ Left err
    Right () -> do
      cfgResult <- loadPluginConfig env
      case cfgResult of
        Left err -> pure $ Left err
        Right cfg -> case Map.lookup plugName (_pcPlugins cfg) of
          Nothing -> pure $ Left $ PluginNotFound plugName
          Just _  -> do
            trackedResult <- gitTracked env
            case trackedResult of
              Left err -> pure $ Left err
              Right allTracked -> do
                let fileSet = Set.fromList relPaths
                    -- Resolve conflicts in all other plugins
                    updatedPlugins = Map.mapWithKey (\k p ->
                      if k == plugName then p
                      else p { _pluginPaths = concatMap (splitPath fileSet allTracked) (_pluginPaths p) }
                      ) (_pcPlugins cfg)
                    -- Add paths to target plugin (no consolidation — avoids
                    -- collapsing siblings into a parent that overlaps other plugins)
                    target = updatedPlugins Map.! plugName
                    existing = _pluginPaths target
                    -- Drop existing paths already covered by new ones
                    kept = filter (\e -> not $ any (`isSubpathOf` e) relPaths) existing
                    -- Only add new paths not already covered by kept
                    toAdd = filter (\p -> not $ any (`isSubpathOf` p) kept) relPaths
                    target' = kept ++ toAdd
                    finalPlugins = Map.insert plugName (target { _pluginPaths = target' }) updatedPlugins
                    newCfg = cfg { _pcPlugins = finalPlugins }
                savePluginConfig env newCfg
                pure $ Right ()

-- | Resolve a single plugin path against a set of reassigned files.
-- Returns replacement paths (empty list = remove, singleton = keep, multiple = expanded).
splitPath :: Set.Set RelPath -> [RelPath] -> RelPath -> [RelPath]
splitPath fileSet allTracked pluginPath
  -- A reassigned path is parent of (or equal to) this plugin path: remove
  | any (\f -> f `isSubpathOf` pluginPath) (Set.toList fileSet) = []
  -- This plugin path is parent of some reassigned file: expand and exclude
  | any (pluginPath `isSubpathOf`) (Set.toList fileSet) =
      let coveredFiles = filter (pluginPath `isSubpathOf`) allTracked
          remaining = filter (`Set.notMember` fileSet) coveredFiles
      in remaining
  -- No conflict
  | otherwise = [pluginPath]

-- | Untrack a file: git rm --cached + remove from plugin.
untrackFile :: GitEnv -> RelPath -> IO (Either DotfError ())
untrackFile env path = do
  let relPath = normalizePath (_geHome env) path
  rmResult <- gitRmCached env relPath
  case rmResult of
    Left err -> pure $ Left err
    Right () -> do
      -- Remove from any plugin that has it
      cfgResult <- loadPluginConfig env
      case cfgResult of
        Left err -> pure $ Left err
        Right cfg -> do
          let newPlugins = Map.map (removePathFromPlugin relPath) (_pcPlugins cfg)
              newCfg = cfg { _pcPlugins = newPlugins }
          savePluginConfig env newCfg
          pure $ Right ()
  where
    removePathFromPlugin p plugin =
      plugin { _pluginPaths = filter (/= p) (_pluginPaths plugin) }

-- | Add a pattern to .gitignore.
ignorePattern :: GitEnv -> T.Text -> IO (Either DotfError ())
ignorePattern env pattern' = do
  result <- try (appendToFile (T.unpack pattern') (gitIgnoreFile env)) :: IO (Either IOException ())
  pure $ either (Left . ConfigError . T.pack . show) Right result

-- | Discover all untracked files, classified by plugin scope.
discoverUntracked :: GitEnv -> IO (Either DotfError UntrackedReport)
discoverUntracked env = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg -> do
      stResult <- loadLocalState env
      case stResult of
        Left err -> pure $ Left err
        Right st -> do
          untrackedResult <- gitUntracked env
          case untrackedResult of
            Left err -> pure $ Left err
            Right files -> do
              -- Only classify against installed plugins
              let installedPlugins = Map.filterWithKey
                    (\k _ -> k `elem` _lsInstalledPlugins st)
                    (_pcPlugins cfg)
                  wlPaths' = _wlPaths (_pcWatchlist cfg)
                  (plugScoped, wl) = classifyUntracked files installedPlugins wlPaths'
              pure $ Right $ UntrackedReport plugScoped wl

-- | Discover untracked files for a specific plugin.
discoverPluginUntracked :: GitEnv -> PluginName -> IO (Either DotfError [RelPath])
discoverPluginUntracked env name = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg ->
      case Map.lookup name (_pcPlugins cfg) of
        Nothing -> pure $ Left $ PluginNotFound name
        Just plugin -> do
          untrackedResult <- gitUntracked env
          case untrackedResult of
            Left err -> pure $ Left err
            Right files ->
              pure $ Right $ filter (matchesPlugin plugin) files
  where
    matchesPlugin plugin fp =
      any (\pp -> pp `isSubpathOf` fp) (_pluginPaths plugin)

-- | Add a path to the watchlist.
-- Note: uses list append (O(n)) — acceptable since watchlists are small in practice.
addWatchPath :: GitEnv -> RelPath -> IO (Either DotfError ())
addWatchPath env path = do
  let relPath = normalizePath (_geHome env) path
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg -> do
      let wl = _pcWatchlist cfg
          newWl = wl { _wlPaths = _wlPaths wl ++ [relPath] }
          newCfg = cfg { _pcWatchlist = newWl }
      savePluginConfig env newCfg
      pure $ Right ()

-- | Remove a path from the watchlist.
removeWatchPath :: GitEnv -> RelPath -> IO (Either DotfError ())
removeWatchPath env path = do
  let relPath = normalizePath (_geHome env) path
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg -> do
      let wl = _pcWatchlist cfg
          newWl = wl { _wlPaths = filter (/= relPath) (_wlPaths wl) }
          newCfg = cfg { _pcWatchlist = newWl }
      savePluginConfig env newCfg
      pure $ Right ()

-- | List current watchlist paths.
listWatchPaths :: GitEnv -> IO (Either DotfError [RelPath])
listWatchPaths env = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err  -> pure $ Left err
    Right cfg -> pure $ Right $ _wlPaths (_pcWatchlist cfg)
