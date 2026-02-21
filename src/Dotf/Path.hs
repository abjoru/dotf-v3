module Dotf.Path (
  normalizePath,
  isSubpathOf,
  findMatchingPlugin,
  relToAbs,
) where

import           Data.List       (find)
import qualified Data.Map.Strict as Map
import           Dotf.Types      (Plugin (..), PluginName, RelPath)
import           System.FilePath (makeRelative, normalise, (</>))

-- | Normalize a path to be relative to the home directory.
-- Handles absolute paths, ~/ paths, and already-relative paths.
normalizePath :: FilePath -> FilePath -> RelPath
normalizePath home fp
  | "~/" `isPrefixOfFP` fp = normalise $ drop 2 fp
  | "/" `isPrefixOfFP` fp  = normalise $ makeRelative home fp
  | otherwise              = normalise fp
  where
    isPrefixOfFP prefix path = take (length prefix) path == prefix

-- | Check if one path is a subpath (prefix) of another.
-- Both paths should be normalized relative paths.
isSubpathOf :: RelPath -> RelPath -> Bool
isSubpathOf parent child =
  let parent' = addTrailingSlash $ normalise parent
      child'  = normalise child
  in parent' `isPrefixOfFP` child' || normalise parent == normalise child
  where
    isPrefixOfFP prefix path = take (length prefix) path == prefix
    addTrailingSlash p
      | null p         = "/"
      | last p == '/'  = p
      | otherwise      = p ++ "/"

-- | Find the plugin that owns a given path.
findMatchingPlugin :: RelPath -> Map.Map PluginName Plugin -> Maybe PluginName
findMatchingPlugin path plugins =
  fst <$> find (\(_, p) -> any (\pp -> pp `isSubpathOf` path || path == normalise pp) (_pluginPaths p)) (Map.toList plugins)

-- | Convert a relative path to an absolute path.
relToAbs :: FilePath -> RelPath -> FilePath
relToAbs home rel = home </> rel
