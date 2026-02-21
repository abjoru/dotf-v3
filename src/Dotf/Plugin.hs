module Dotf.Plugin (
  -- * Pure
  resolveDependencies,
  checkRemoveSafety,
  validatePaths,

  managedPaths,

  -- * IO
  listPlugins,
  pluginInfo,
  pluginFiles,
  createPlugin,
  deletePlugin,
  installPlugins,
  removePlugins,
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Dotf.Config
import           Dotf.Git
import           Dotf.Path       (isSubpathOf)
import           Dotf.State
import           Dotf.Types

--------------------
-- Pure functions --
--------------------

-- | Topological sort of plugin dependencies.
-- Returns all plugins needed (including transitive deps) in install order.
-- Two-pass: DFS to collect in dependency order, then deduplicate keeping first occurrence.
resolveDependencies :: Map.Map PluginName Plugin
                    -> [PluginName]
                    -> Either DotfError [PluginName]
resolveDependencies plugins targets = nubKeepFirst <$> go Set.empty targets
  where
    go _       []     = Right []
    go visited (x:xs)
      | Set.member x visited = go visited xs
      | otherwise = case Map.lookup x plugins of
          Nothing -> Left $ PluginNotFound x
          Just p  -> do
            let visited' = Set.insert x visited
            -- Check for cycles: deps that are already being visited
            let depCycles = filter (`Set.member` visited') (_pluginDepends p)
            case depCycles of
              (c:_) -> Left $ DependencyError $
                T.concat ["Circular dependency detected involving: ", c]
              [] -> do
                deps <- go visited' (_pluginDepends p)
                rest <- go (Set.union visited' (Set.fromList (map fst (zip deps deps)))) xs
                pure $ deps ++ [x] ++ rest

    nubKeepFirst = go' Set.empty
      where
        go' _    []     = []
        go' seen (y:ys)
          | Set.member y seen = go' seen ys
          | otherwise         = y : go' (Set.insert y seen) ys

-- | Check if removing plugins is safe (no installed dependents remain).
checkRemoveSafety :: Map.Map PluginName Plugin
                  -> [PluginName]  -- ^ Currently installed
                  -> [PluginName]  -- ^ To remove
                  -> Either DotfError ()
checkRemoveSafety plugins installed toRemove =
  let removeSet  = Set.fromList toRemove
      remaining  = filter (`Set.notMember` removeSet) installed
      dependents = [ (dep, name)
                   | name <- remaining
                   , Just p <- [Map.lookup name plugins]
                   , dep <- _pluginDepends p
                   , dep `Set.member` removeSet
                   ]
  in case dependents of
    [] -> Right ()
    ((dep, name):_) -> Left $ DependencyError $
      T.concat ["Cannot remove ", dep, ": required by installed plugin ", name]

-- | Validate no path appears in multiple plugins.
-- Uses a Map accumulator for O(n log n) duplicate detection.
validatePaths :: Map.Map PluginName Plugin -> Either DotfError ()
validatePaths = checkDuplicates . concatMap expandPlugin . Map.toList
  where
    expandPlugin (name, plugin) = map (\p -> (name, p)) (_pluginPaths plugin)

    checkDuplicates pairs = go Map.empty pairs
      where
        go _ [] = Right ()
        go seen ((name, path):rest) =
          case Map.lookup path seen of
            Just other -> Left $ PathConflict name other path
            Nothing    -> go (Map.insert path name seen) rest

--------------------
-- IO functions   --
--------------------

-- | List all plugins with their install status.
listPlugins :: PluginConfig -> LocalState -> [(Plugin, Bool)]
listPlugins cfg st =
  let installed = Set.fromList (_lsInstalledPlugins st)
  in [ (p, _pluginName p `Set.member` installed)
     | p <- Map.elems (_pcPlugins cfg)
     ]

-- | Get detailed info about a plugin.
pluginInfo :: GitEnv -> PluginName -> PluginConfig -> LocalState
           -> IO (Either DotfError (Plugin, Bool, [RelPath]))
pluginInfo env name cfg st =
  case Map.lookup name (_pcPlugins cfg) of
    Nothing -> pure $ Left $ PluginNotFound name
    Just p  -> do
      let installed = name `Set.member` Set.fromList (_lsInstalledPlugins st)
      files <- pluginFiles env p
      case files of
        Left err -> pure $ Left err
        Right fs -> pure $ Right (p, installed, fs)

-- | List actual files for a plugin (git tracked files matching plugin paths).
pluginFiles :: GitEnv -> Plugin -> IO (Either DotfError [RelPath])
pluginFiles env plugin = do
  tracked <- gitTracked env
  case tracked of
    Left err -> pure $ Left err
    Right allFiles ->
      pure $ Right $ filter (matchesPlugin plugin) allFiles

-- | Create a new empty plugin definition.
createPlugin :: GitEnv -> PluginName -> Maybe Text -> IO (Either DotfError ())
createPlugin env name desc = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg ->
      if Map.member name (_pcPlugins cfg)
      then pure $ Left $ ValidationError $ T.concat ["Plugin already exists: ", name]
      else do
        let newPlugin = Plugin name desc [] [] Nothing [] [] []
            newCfg = cfg { _pcPlugins = Map.insert name newPlugin (_pcPlugins cfg) }
        savePluginConfig env newCfg
        pure $ Right ()

-- | Delete a plugin definition (must have no files).
deletePlugin :: GitEnv -> PluginName -> IO (Either DotfError ())
deletePlugin env name = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg ->
      case Map.lookup name (_pcPlugins cfg) of
        Nothing -> pure $ Left $ PluginNotFound name
        Just p
          | not (null (_pluginPaths p)) ->
            pure $ Left $ ValidationError $
              T.concat ["Plugin ", name, " still has assigned paths"]
          | otherwise -> do
            let newCfg = cfg { _pcPlugins = Map.delete name (_pcPlugins cfg) }
            savePluginConfig env newCfg
            pure $ Right ()

-- | Install plugins: resolve deps, update sparse checkout, save state.
installPlugins :: GitEnv -> [PluginName] -> IO (Either DotfError ())
installPlugins env names = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg -> do
      stResult <- loadLocalState env
      case stResult of
        Left err -> pure $ Left err
        Right st -> case resolveDependencies (_pcPlugins cfg) names of
          Left err -> pure $ Left err
          Right resolved -> do
            let installedSet = Set.fromList (_lsInstalledPlugins st)
                allToInstall = filter (`Set.notMember` installedSet) resolved
                allInstalled = _lsInstalledPlugins st ++ allToInstall
                allPaths = concatMap (pluginPathsFor cfg) allInstalled
                sparseTargets = managedPaths ++ allPaths
            result <- gitSparseCheckoutSet env sparseTargets
            case result of
              Left err -> pure $ Left err
              Right () -> do
                let newSt = st { _lsInstalledPlugins = allInstalled }
                saveLocalState env newSt
                pure $ Right ()

-- | Remove plugins: check safety, rebuild sparse checkout, save state.
removePlugins :: GitEnv -> [PluginName] -> IO (Either DotfError ())
removePlugins env names = do
  cfgResult <- loadPluginConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg -> do
      stResult <- loadLocalState env
      case stResult of
        Left err -> pure $ Left err
        Right st -> case checkRemoveSafety (_pcPlugins cfg) (_lsInstalledPlugins st) names of
          Left err -> pure $ Left err
          Right () -> do
            let removeSet = Set.fromList names
                remaining = filter (`Set.notMember` removeSet) (_lsInstalledPlugins st)
                allPaths = concatMap (pluginPathsFor cfg) remaining
                sparseTargets = managedPaths ++ allPaths
            result <- gitSparseCheckoutSet env sparseTargets
            case result of
              Left err -> pure $ Left err
              Right () -> do
                let newSt = st { _lsInstalledPlugins = remaining }
                saveLocalState env newSt
                pure $ Right ()

-----------
-- Utils --
-----------

matchesPlugin :: Plugin -> FilePath -> Bool
matchesPlugin plugin fp = any (`isSubpathOf` fp) (_pluginPaths plugin)

-- | Paths always included in sparse checkout regardless of profile.
managedPaths :: [FilePath]
managedPaths = [".config/dotf/", ".gitignore"]

pluginPathsFor :: PluginConfig -> PluginName -> [FilePath]
pluginPathsFor cfg name =
  case Map.lookup name (_pcPlugins cfg) of
    Nothing -> []
    Just p  -> _pluginPaths p
