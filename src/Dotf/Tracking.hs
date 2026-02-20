module Dotf.Tracking (
  -- * Pure
  classifyUntracked,

  -- * IO
  trackFile,
  untrackFile,
  ignorePattern,
  discoverUntracked,
  discoverPluginUntracked,
  addWatchPath,
  removeWatchPath,
  listWatchPaths,
) where

import qualified Data.Map.Strict  as Map
import qualified Data.Text        as T
import           Dotf.Config
import           Dotf.Git
import           Dotf.Path        (isSubpathOf, normalizePath)
import           Dotf.State
import           Dotf.Types
import           Dotf.Utils       (appendToFile, gitIgnoreFile)

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

-- | Track a file: git add + assign to plugin.
trackFile :: GitEnv -> RelPath -> Maybe PluginName -> IO (Either DotfError ())
trackFile env path mPlugin = do
  -- Normalize the path
  let relPath = normalizePath (_geHome env) path
  -- Git add
  addResult <- gitAdd env relPath
  case addResult of
    Left err -> pure $ Left err
    Right () -> do
      -- Assign to plugin if specified
      case mPlugin of
        Nothing -> pure $ Right ()
        Just plugName -> do
          cfgResult <- loadPluginConfig env
          case cfgResult of
            Left err -> pure $ Left err
            Right cfg ->
              case Map.lookup plugName (_pcPlugins cfg) of
                Nothing -> pure $ Left $ PluginNotFound plugName
                Just plugin -> do
                  let updatedPlugin = plugin { _pluginPaths = _pluginPaths plugin ++ [relPath] }
                      newCfg = cfg { _pcPlugins = Map.insert plugName updatedPlugin (_pcPlugins cfg) }
                  savePluginConfig env newCfg
                  pure $ Right ()

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
  appendToFile (T.unpack pattern') (gitIgnoreFile env)
  pure $ Right ()

-- | Discover all untracked files, classified by plugin scope.
discoverUntracked :: GitEnv -> IO (Either DotfError UntrackedReport)
discoverUntracked env = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg -> do
      st <- loadLocalState env
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
