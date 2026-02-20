module Dotf.Plugin (
  -- * Pure
  resolveDependencies,
  checkRemoveSafety,
  validatePaths,

  -- * IO
  listPlugins,
  pluginInfo,
  pluginFiles,
  createPlugin,
  deletePlugin,
  installPlugins,
  removePlugins,
) where

import qualified Data.Map.Strict  as Map
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Dotf.Config
import           Dotf.Git
import           Dotf.State
import           Dotf.Types

--------------------
-- Pure functions --
--------------------

-- | Topological sort of plugin dependencies.
-- Returns all plugins needed (including transitive deps) in install order.
resolveDependencies :: Map.Map PluginName Plugin
                    -> [PluginName]
                    -> Either DotfError [PluginName]
resolveDependencies plugins targets = go [] [] targets
  where
    go _       resolved []     = Right (reverse resolved)
    go visited resolved (x:xs)
      | x `elem` resolved = go visited resolved xs
      | x `elem` visited  = Left $ DependencyError $
          T.concat ["Circular dependency detected involving: ", x]
      | otherwise = case Map.lookup x plugins of
          Nothing -> Left $ PluginNotFound x
          Just p  -> do
            deps <- go (x:visited) resolved (_pluginDepends p)
            go visited (x : deps) xs

-- | Check if removing plugins is safe (no installed dependents remain).
checkRemoveSafety :: Map.Map PluginName Plugin
                  -> [PluginName]  -- ^ Currently installed
                  -> [PluginName]  -- ^ To remove
                  -> Either DotfError ()
checkRemoveSafety plugins installed toRemove =
  let remaining = filter (`notElem` toRemove) installed
      dependents = [ (dep, name)
                   | name <- remaining
                   , Just p <- [Map.lookup name plugins]
                   , dep <- _pluginDepends p
                   , dep `elem` toRemove
                   ]
  in case dependents of
    [] -> Right ()
    ((dep, name):_) -> Left $ DependencyError $
      T.concat ["Cannot remove ", dep, ": required by installed plugin ", name]

-- | Validate no path appears in multiple plugins.
validatePaths :: Map.Map PluginName Plugin -> Either DotfError ()
validatePaths = checkDuplicates . concatMap expandPlugin . Map.toList
  where
    expandPlugin (name, plugin) = map (\p -> (name, p)) (_pluginPaths plugin)

    checkDuplicates [] = Right ()
    checkDuplicates ((name, path):rest) =
      case lookup path (map (\(n, p) -> (p, n)) rest) of
        Just other -> Left $ PathConflict name other path
        Nothing    -> checkDuplicates rest

--------------------
-- IO functions   --
--------------------

-- | List all plugins with their install status.
listPlugins :: PluginConfig -> LocalState -> [(Plugin, Bool)]
listPlugins cfg st =
  [ (p, _pluginName p `elem` _lsInstalledPlugins st)
  | p <- Map.elems (_pcPlugins cfg)
  ]

-- | Get detailed info about a plugin.
pluginInfo :: GitEnv -> PluginName -> PluginConfig -> LocalState
           -> IO (Either DotfError (Plugin, Bool, [RelPath]))
pluginInfo env name cfg st =
  case Map.lookup name (_pcPlugins cfg) of
    Nothing -> pure $ Left $ PluginNotFound name
    Just p  -> do
      let installed = name `elem` _lsInstalledPlugins st
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
        let newPlugin = Plugin name desc [] [] Nothing
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
      st <- loadLocalState env
      case resolveDependencies (_pcPlugins cfg) names of
        Left err -> pure $ Left err
        Right resolved -> do
          let allToInstall = filter (`notElem` _lsInstalledPlugins st) resolved
              allInstalled = _lsInstalledPlugins st ++ allToInstall
              allPaths = concatMap (pluginPathsFor cfg) allInstalled
              sparseTargets = ".config/dotf/" : allPaths
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
      st <- loadLocalState env
      case checkRemoveSafety (_pcPlugins cfg) (_lsInstalledPlugins st) names of
        Left err -> pure $ Left err
        Right () -> do
          let remaining = filter (`notElem` names) (_lsInstalledPlugins st)
              allPaths = concatMap (pluginPathsFor cfg) remaining
              sparseTargets = ".config/dotf/" : allPaths
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
matchesPlugin plugin fp =
  any (\pp -> pp `isPrefixOf'` fp || fp == pp) (_pluginPaths plugin)
  where
    isPrefixOf' prefix path =
      let prefix' = if null prefix || last prefix == '/' then prefix
                    else prefix ++ "/"
      in take (length prefix') path == prefix'

pluginPathsFor :: PluginConfig -> PluginName -> [FilePath]
pluginPathsFor cfg name =
  case Map.lookup name (_pcPlugins cfg) of
    Nothing -> []
    Just p  -> _pluginPaths p
