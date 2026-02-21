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

-- | Assign files to a plugin, checking for cross-plugin path conflicts.
-- Returns PathConflict if any file is already covered by another plugin.
reassignFiles :: GitEnv -> [RelPath] -> PluginName -> IO (Either DotfError ())
reassignFiles env paths plugName = do
  let relPaths = map (normalizePath (_geHome env)) paths
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
          Just _  -> case findConflict relPaths plugName (_pcPlugins cfg) of
            Just err -> pure $ Left err
            Nothing  -> do
              let target   = (_pcPlugins cfg) Map.! plugName
                  existing = _pluginPaths target
                  kept     = filter (\e -> not $ any (`isSubpathOf` e) relPaths) existing
                  toAdd    = filter (\p -> not $ any (`isSubpathOf` p) kept) relPaths
                  target'  = kept ++ toAdd
                  newPlugins = Map.insert plugName (target { _pluginPaths = target' }) (_pcPlugins cfg)
                  newCfg   = cfg { _pcPlugins = newPlugins }
              savePluginConfig env newCfg
              pure $ Right ()

-- | Check if any path is already covered by a different plugin.
findConflict :: [RelPath] -> PluginName -> Map.Map PluginName Plugin -> Maybe DotfError
findConflict paths target plugins =
  let others = Map.toList $ Map.delete target plugins
      check fp = case [ owner | (owner, p) <- others
                       , any (\pp -> pp `isSubpathOf` fp || fp `isSubpathOf` pp) (_pluginPaths p)
                       ] of
                   (owner:_) -> Just $ PathConflict target owner fp
                   []        -> Nothing
  in case concatMap (\fp -> maybe [] (:[]) (check fp)) paths of
       (err:_) -> Just err
       []      -> Nothing

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
          let installedPlugins = Map.filterWithKey
                    (\k _ -> k `elem` _lsInstalledPlugins st)
                    (_pcPlugins cfg)
              wlPaths' = _wlPaths (_pcWatchlist cfg)
              scope = concatMap _pluginPaths (Map.elems installedPlugins) ++ wlPaths'
          untrackedResult <- gitUntracked env scope
          case untrackedResult of
            Left err -> pure $ Left err
            Right files -> do
              let (plugScoped, wl) = classifyUntracked files installedPlugins wlPaths'
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
          untrackedResult <- gitUntracked env (_pluginPaths plugin)
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
