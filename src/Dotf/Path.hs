module Dotf.Path (
  normalizePath,
  isSubpathOf,
  findMatchingPlugin,
  relToAbs,
  consolidatePluginPaths,
) where

import           Data.List       (find, isPrefixOf, nub, sort)
import qualified Data.Map.Strict as Map
import           Dotf.Types      (Plugin (..), PluginName, RelPath)
import           System.FilePath (makeRelative, normalise, takeDirectory, (</>))

-- | Normalize a path to be relative to the home directory.
-- Handles absolute paths, ~/ paths, and already-relative paths.
normalizePath :: FilePath -> FilePath -> RelPath
normalizePath home fp
  | "~/" `isPrefixOf` fp = normalise $ drop 2 fp
  | "/" `isPrefixOf` fp  = normalise $ makeRelative home fp
  | otherwise            = normalise fp

-- | Check if one path is a subpath (prefix) of another.
-- Both paths should be normalized relative paths.
isSubpathOf :: RelPath -> RelPath -> Bool
isSubpathOf parent child =
  let parent' = addTrailingSlash $ normalise parent
      child'  = normalise child
  in parent' `isPrefixOf` child' || normalise parent == normalise child
  where
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

-- | Consolidate plugin paths by replacing sibling files with their parent dir.
-- Returns @(pluginName, oldPaths, newPaths)@ for each changed plugin.
-- Safety: rejects consolidation if any other plugin or watchlist path overlaps
-- the candidate parent directory.
consolidatePluginPaths
  :: Map.Map PluginName Plugin
  -> [RelPath]                        -- ^ watchlist paths
  -> [(PluginName, [RelPath], [RelPath])]
consolidatePluginPaths plugins watchPaths =
  let initial = Map.map _pluginPaths plugins
      final   = iterateConsolidate 10 plugins watchPaths initial
  in [ (name, old, new')
     | (name, new') <- Map.toList final
     , let old = _pluginPaths (plugins Map.! name)
     , sort old /= sort new'
     ]

-- | Iterate consolidation rounds until stable or max rounds reached.
iterateConsolidate
  :: Int
  -> Map.Map PluginName Plugin
  -> [RelPath]
  -> Map.Map PluginName [RelPath]
  -> Map.Map PluginName [RelPath]
iterateConsolidate 0 _ _ current = current
iterateConsolidate n plugins watchPaths current =
  let next = Map.mapWithKey (consolidateOne current watchPaths) current
  in if next == current
       then current
       else iterateConsolidate (n - 1) plugins watchPaths next

-- | One round of consolidation for a single plugin.
consolidateOne
  :: Map.Map PluginName [RelPath]     -- ^ all current path sets
  -> [RelPath]                        -- ^ watchlist
  -> PluginName
  -> [RelPath]
  -> [RelPath]
consolidateOne allPaths watchPaths name paths =
  let deduped  = dedupSubsumed paths
      grouped  = groupByParent deduped
      otherPaths = concat [ ps | (n, ps) <- Map.toList allPaths, n /= name ]
  in concatMap (tryConsolidate otherPaths watchPaths) grouped

-- | Remove paths already subsumed by a broader path in the same list.
dedupSubsumed :: [RelPath] -> [RelPath]
dedupSubsumed paths =
  [ p | p <- paths
  , not $ any (\q -> q /= p && q `isSubpathOf` p) paths
  ]

-- | Group paths by their parent directory, returning (parentDir, members).
groupByParent :: [RelPath] -> [(RelPath, [RelPath])]
groupByParent paths =
  let parentOf = normalise . takeDirectory . stripTrailingSlash
      parents  = nub $ map parentOf paths
  in [ (parent, members)
     | parent <- parents
     , let members = filter (\p -> parentOf p == parent) paths
     ]

-- | Remove trailing path separator from a path.
stripTrailingSlash :: FilePath -> FilePath
stripTrailingSlash p
  | length p > 1 && last p == '/' = init p
  | otherwise                     = p

-- | Try to consolidate a group of sibling paths to their parent dir.
-- Only consolidates groups of 2+ siblings when safe.
tryConsolidate :: [RelPath] -> [RelPath] -> (RelPath, [RelPath]) -> [RelPath]
tryConsolidate otherPaths watchPaths (parent, members)
  | length members < 2 = members
  | conflictsWithOthers = members
  | conflictsWithWatch  = members
  | parent == "."       = members
  | otherwise           = [parent]
  where
    conflictsWithOthers = any (\op -> parent `isSubpathOf` op) otherPaths
    conflictsWithWatch  = any (\wp -> parent `isSubpathOf` wp) watchPaths
