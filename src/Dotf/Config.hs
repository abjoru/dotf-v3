module Dotf.Config (
  loadPluginConfig,
  savePluginConfig,
  loadProfileConfig,
  saveProfileConfig,
  defaultPluginConfig,
  defaultProfileConfig,
  validatePluginConfig,
) where

import qualified Data.Map.Strict  as Map
import           Data.Text        (pack)
import qualified Data.Yaml        as Y
import           Dotf.Types
import           Dotf.Utils       (pluginsFile, profilesFile)
import           System.Directory (doesFileExist)

-- Note: validatePluginConfig cannot delegate to Plugin.validatePaths because
-- Plugin imports Config, which would create a circular dependency.

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
-- Uses a Map accumulator for O(n log n) duplicate detection.
validatePluginConfig :: PluginConfig -> Either DotfError ()
validatePluginConfig cfg = go Map.empty allPaths
  where
    allPaths :: [(PluginName, FilePath)]
    allPaths = concatMap expandPlugin $ Map.toList (_pcPlugins cfg)

    expandPlugin :: (PluginName, Plugin) -> [(PluginName, FilePath)]
    expandPlugin (name, plugin) = map (\p -> (name, p)) (_pluginPaths plugin)

    go :: Map.Map FilePath PluginName -> [(PluginName, FilePath)] -> Either DotfError ()
    go _ [] = Right ()
    go seen ((name, path):rest) =
      case Map.lookup path seen of
        Just other -> Left $ PathConflict name other path
        Nothing    -> go (Map.insert path name seen) rest
