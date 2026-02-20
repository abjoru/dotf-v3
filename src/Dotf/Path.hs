module Dotf.Path (
  normalizePath,
  isSubpathOf,
  consolidatePaths,
  findMatchingPlugin,
  relToAbs,
) where

import qualified Data.Map.Strict as Map
import           Dotf.Types      (Plugin (..), PluginName, RelPath)
import           System.FilePath (joinPath, makeRelative, normalise,
                                  splitDirectories, (</>))

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

-- | Add a path to a list, consolidating shared directory prefixes.
-- Skips if already covered. Collapses to common prefix (>= 2 components).
consolidatePaths :: [RelPath] -> RelPath -> [RelPath]
consolidatePaths existing new
  | any (`isSubpathOf` new) existing = existing
  | otherwise = case findCollapse existing new of
      Nothing     -> existing ++ [new]
      Just prefix -> filter (\e -> not (prefix `isSubpathOf` e)) existing ++ [prefix]

-- | Find common directory prefix between new path and first qualifying existing path.
findCollapse :: [RelPath] -> RelPath -> Maybe RelPath
findCollapse [] _ = Nothing
findCollapse (e:es) new = case commonDirPrefix e new of
  Just p  -> Just p
  Nothing -> findCollapse es new

-- | Longest common directory prefix of two paths.
-- Returns Nothing if fewer than 2 shared components.
commonDirPrefix :: RelPath -> RelPath -> Maybe RelPath
commonDirPrefix a b =
  let segsA  = splitDirectories (normalise a)
      segsB  = splitDirectories (normalise b)
      common = map fst $ takeWhile (uncurry (==)) $ zip segsA segsB
  in if length common >= 2
     then Just (normalise (joinPath common))
     else Nothing

-- | Find the plugin that owns a given path.
findMatchingPlugin :: RelPath -> Map.Map PluginName Plugin -> Maybe PluginName
findMatchingPlugin path = fst . Map.foldlWithKey' check (Nothing, False)
  where
    check (found, True) _ _  = (found, True)
    check _           k plugin =
      if any (\pp -> pp `isSubpathOf` path || path == normalise pp) (_pluginPaths plugin)
      then (Just k, True)
      else (Nothing, False)

-- | Convert a relative path to an absolute path.
relToAbs :: FilePath -> RelPath -> FilePath
relToAbs home rel = home </> rel
