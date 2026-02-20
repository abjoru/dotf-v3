module Dotf.Config (
  loadPluginConfig,
  savePluginConfig,
  loadProfileConfig,
  saveProfileConfig,
  defaultPluginConfig,
  defaultProfileConfig,
  validatePluginConfig,
) where

import qualified Data.Map.Strict as Map
import           Data.Text       (pack)
import qualified Data.Yaml       as Y
import           Dotf.Types
import           Dotf.Utils      (pluginsFile, profilesFile)
import           System.Directory (doesFileExist)

-- | Load plugin config from ~/.config/dotf/plugins.yaml.
loadPluginConfig :: GitEnv -> IO (Either DotfError PluginConfig)
loadPluginConfig env = do
  let path = pluginsFile env
  exists <- doesFileExist path
  if not exists
    then pure $ Right defaultPluginConfig
    else do
      result <- Y.decodeFileEither path
      case result of
        Left err  -> pure $ Left $ ConfigError $ pack $ Y.prettyPrintParseException err
        Right cfg -> pure $ Right cfg

-- | Save plugin config to disk.
savePluginConfig :: GitEnv -> PluginConfig -> IO ()
savePluginConfig env cfg = Y.encodeFile (pluginsFile env) cfg

-- | Load profile config from ~/.config/dotf/profiles.yaml.
loadProfileConfig :: GitEnv -> IO (Either DotfError ProfileConfig)
loadProfileConfig env = do
  let path = profilesFile env
  exists <- doesFileExist path
  if not exists
    then pure $ Right defaultProfileConfig
    else do
      result <- Y.decodeFileEither path
      case result of
        Left err  -> pure $ Left $ ConfigError $ pack $ Y.prettyPrintParseException err
        Right cfg -> pure $ Right cfg

-- | Save profile config to disk.
saveProfileConfig :: GitEnv -> ProfileConfig -> IO ()
saveProfileConfig env cfg = Y.encodeFile (profilesFile env) cfg

-- | Default empty plugin config.
defaultPluginConfig :: PluginConfig
defaultPluginConfig = PluginConfig Map.empty (Watchlist [])

-- | Default empty profile config.
defaultProfileConfig :: ProfileConfig
defaultProfileConfig = ProfileConfig Map.empty

-- | Validate that no path appears in multiple plugins.
validatePluginConfig :: PluginConfig -> Either DotfError ()
validatePluginConfig cfg = checkDuplicates allPaths
  where
    allPaths :: [(PluginName, FilePath)]
    allPaths = concatMap expandPlugin $ Map.toList (_pcPlugins cfg)

    expandPlugin :: (PluginName, Plugin) -> [(PluginName, FilePath)]
    expandPlugin (name, plugin) = map (\p -> (name, p)) (_pluginPaths plugin)

    checkDuplicates :: [(PluginName, FilePath)] -> Either DotfError ()
    checkDuplicates [] = Right ()
    checkDuplicates ((name, path):rest) =
      case lookup path (map (\(n, p) -> (p, n)) rest) of
        Just other -> Left $ PathConflict name other path
        Nothing    -> checkDuplicates rest
