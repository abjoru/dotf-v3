{-# LANGUAGE RankNTypes #-}
module Dotf.Tui.Types (
  -- * State
  State(..),
  Tab(..),
  Focus(..),
  Popup(..),
  RName(..),

  -- * Item types
  GroupItem(..),
  UntrackedItem(..),
  SaveItem(..),
  PkgItem(..),

  -- * Custom event
  DEvent(..),
  ConfirmAction(..),

  -- * State builders
  buildState,
  syncAll,
  syncDotfiles,
  syncPlugins,
  syncProfiles,

  -- * List helpers
  buildGroupedList,
  buildUntrackedList,
  rebuildGroupedList,

  -- * Popup helpers
  openSavePopup,
  openAssignPopup,
  openIgnorePopup,
  openNewPluginPopup,
  openNewProfilePopup,
  openPackagePopup,
  pathSegments,

  -- * Lenses
  stEnv, stPluginConfig, stProfileConfig, stLocalState, stAllTracked,
  stTab, stFocus, stPopup,
  stTrackedList, stUntrackedList, stCollapsed, stSelected, stStagedPairs, stUnstagedPairs,
  stPluginListW, stPluginFiles,
  stProfileListW,
  stSaveItems, stCommitEditor,
  stAssignFiles, stAssignList, stAssignEditing, stAssignEditor,
  stIgnoreList,
  stNewPluginName, stNewPluginDesc,
  stNewProfileName, stNewProfilePlugins,
  stAiMenuList,
  stPkgItems, stPkgDistro,
  stDetailAdvanced,
  stFrozen,
  stFilterEditor, stFilterActive,
  stConfirm,
  stUncommitted, stAhead, stBehind, stAssignedCount, stTotalCount,
  stError,
  siPath, siSelected, siIsStaged,
  piName, piSelected, piIsCask,
) where

import qualified Brick.Widgets.Edit       as E
import qualified Brick.Widgets.List       as L
import           Control.Concurrent.Async (concurrently)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Dotf.Config
import           Dotf.Git
import           Dotf.Packages            (Distro (..), detectDistro)
import           Dotf.Path                (findMatchingPlugin, isSubpathOf)
import           Dotf.Plugin              (listPlugins, managedPaths)
import           Dotf.Profile             (checkCoverage, listProfiles)
import           Dotf.State
import           Dotf.Tracking            (classifyUntracked, listFrozen)
import           Dotf.Types
import           Lens.Micro               ((&), (.~), (^.))
import           Lens.Micro.TH            (makeLenses)
import           System.Directory         (doesPathExist)
import           System.FilePath          ((</>))

-----------
-- Types --
-----------

data Tab = DotfilesTab | PluginsTab | ProfilesTab
  deriving (Eq, Show, Ord)

data Focus
  = FTracked
  | FUntracked
  | FPluginList
  | FPluginDetail
  | FProfileList
  | FProfileDetail
  | FSaveList
  | FSaveEditor
  | FAssignList
  | FAssignEditor
  | FIgnoreList
  | FFilterEditor
  | FNewPluginName
  | FNewPluginDesc
  | FNewProfileName
  | FNewProfilePlugins
  | FPkgList
  | FAiMenu
  deriving (Eq, Show, Ord)

data Popup = SavePopup | AssignPopup | IgnorePopup | FilterPopup | NewPluginPopup | NewProfilePopup | PackagePopup | AiMenuPopup | HelpPopup
  deriving (Eq, Show, Ord)

-- | Resource names for Brick widgets.
data RName
  = RTrackedList
  | RUntrackedList
  | RPluginList
  | RProfileList
  | RSaveList
  | RAssignList
  | RCommitEditor
  | RAssignEditor
  | RIgnoreList
  | RFilterEditor
  | RPluginDetail
  | RProfileDetail
  | RNewPluginName
  | RNewPluginDesc
  | RNewProfileName
  | RNewProfilePlugins
  | RPkgList
  | RAiMenuList
  | RHelpViewport
  deriving (Eq, Show, Ord)

-- | Grouped item in tracked list: headers or file entries.
data GroupItem
  = GHeader PluginName Bool Int  -- ^ plugin name, collapsed?, dirty count
  | GStaged RelPath PluginName
  | GUnstaged RelPath FileStatus PluginName
  | GTracked RelPath PluginName
  | GUnassignedHeader
  | GUnassignedFile RelPath
  deriving (Eq, Show)

-- | Untracked file item.
data UntrackedItem
  = UPluginHeader PluginName
  | UPluginFile RelPath PluginName
  | UWatchlistHeader
  | UWatchlistFile RelPath
  deriving (Eq, Show)

-- | Save popup item: togglable file.
data SaveItem = SaveItem
  { _siPath     :: RelPath
  , _siSelected :: Bool
  , _siIsStaged :: Bool
  } deriving (Eq, Show)

-- | Package popup item: toggleable package.
data PkgItem = PkgItem
  { _piName     :: Text
  , _piSelected :: Bool
  , _piIsCask   :: Bool
  } deriving (Eq, Show)

-- | Custom event type for Brick.
data DEvent = Tick
  deriving (Eq, Show)

-- | Confirmable actions.
data ConfirmAction
  = ConfirmUntrack [RelPath]
  | ConfirmDeletePlugin PluginName
  | ConfirmRemovePlugin PluginName
  | ConfirmDeleteProfile ProfileName
  | ConfirmDeactivateProfile
  | ConfirmFreeze [RelPath]
  | ConfirmUnfreeze [RelPath]
  deriving (Eq, Show)

-----------
-- State --
-----------

data State = State
  { _stEnv               :: GitEnv
  , _stPluginConfig      :: PluginConfig
  , _stProfileConfig     :: ProfileConfig
  , _stLocalState        :: LocalState
  , _stAllTracked        :: [RelPath]

  -- Navigation
  , _stTab               :: Tab
  , _stFocus             :: Focus
  , _stPopup             :: Maybe Popup

  -- Dotfiles tab
  , _stTrackedList       :: L.List RName GroupItem
  , _stUntrackedList     :: L.List RName UntrackedItem
  , _stCollapsed         :: Set PluginName
  , _stSelected          :: Set RelPath
  , _stFrozen            :: Set RelPath
  , _stStagedPairs       :: [(RelPath, FileStatus)]
  , _stUnstagedPairs     :: [(RelPath, FileStatus)]

  -- Plugins tab
  , _stPluginListW       :: L.List RName (Plugin, Bool)
  , _stPluginFiles       :: Map.Map PluginName [RelPath]

  -- Profiles tab
  , _stProfileListW      :: L.List RName (Profile, Bool)

  -- Save popup
  , _stSaveItems         :: L.List RName SaveItem
  , _stCommitEditor      :: E.Editor String RName

  -- Assign popup
  , _stAssignFiles       :: [RelPath]
  , _stAssignList        :: L.List RName (PluginName, Bool)
  , _stAssignEditing     :: Bool
  , _stAssignEditor      :: E.Editor String RName

  -- Ignore popup
  , _stIgnoreList        :: L.List RName FilePath

  -- New plugin popup
  , _stNewPluginName     :: E.Editor String RName
  , _stNewPluginDesc     :: E.Editor String RName

  -- AI menu popup
  , _stAiMenuList        :: L.List RName (String, String)

  -- Package popup
  , _stPkgItems          :: L.List RName PkgItem
  , _stPkgDistro         :: Distro

  -- Detail toggle
  , _stDetailAdvanced    :: Bool

  -- New profile popup
  , _stNewProfileName    :: E.Editor String RName
  , _stNewProfilePlugins :: L.List RName (PluginName, Bool)

  -- Filter
  , _stFilterEditor      :: E.Editor String RName
  , _stFilterActive      :: Bool

  -- Confirm dialog
  , _stConfirm           :: Maybe (String, ConfirmAction)

  -- Status bar
  , _stUncommitted       :: Int
  , _stAhead             :: Int
  , _stBehind            :: Int
  , _stAssignedCount     :: Int
  , _stTotalCount        :: Int

  -- Error
  , _stError             :: Maybe [String]
  }

makeLenses ''SaveItem
makeLenses ''PkgItem
makeLenses ''State

--------------------
-- State builders --
--------------------

-- | Build initial TUI state from a GitEnv.
buildState :: GitEnv -> IO State
buildState env = do
  -- Load configs first (fast YAML reads, needed to scope untracked scan)
  (pcfgE, prfE, lsE) <- concurrently3
    (loadPluginConfig env) (loadProfileConfig env) (loadLocalState env)

  let pcfg = either (const defaultPluginConfig) id pcfgE
      prf  = either (const defaultProfileConfig) id prfE
      ls   = either (const defaultLocalState) id lsE
      cfgErrors = either (\e -> [displayError e]) (const []) pcfgE
               ++ either (\e -> [displayError e]) (const []) prfE
               ++ either (\e -> [displayError e]) (const []) lsE
      initError = if null cfgErrors then Nothing else Just cfgErrors
      untrackedScope = scopePaths pcfg

  -- Detect distro for package display
  dist <- detectDistro

  -- Run git queries in parallel (untracked scoped to plugin + watchlist paths)
  ((trackedE, stagedE), (unstagedE, untrkE), (ah, bh)) <-
    concurrently3
      (concurrently (gitTracked env) (gitTrackedStaged env))
      (concurrently (gitTrackedUnstaged env) (gitUntracked env untrackedScope))
      (concurrently (gitAhead env) (gitBehind env))

  frozenE <- listFrozen env
  let frozenSet = Set.fromList $ either (const []) id frozenE

  let tracked  = either (const []) id trackedE
      staged   = either (const []) id stagedE
      unstaged = either (const []) id unstagedE
      untrk    = either (const []) id untrkE
      uncommitted = length staged + length unstaged

  -- Classify tracked files with status
  stagedStatuses <- mapM (checkStatus env) staged
  unstagedStatuses <- mapM (checkStatus env) unstaged

  let plugins     = _pcPlugins pcfg
      collapsed   = Set.fromList (Map.keys plugins)
      groupedList = buildGroupedList plugins tracked stagedStatuses unstagedStatuses collapsed Nothing
      untrkList   = buildUntrackedList plugins (_wlPaths $ _pcWatchlist pcfg) untrk

  -- Plugin files cache (reuse already-fetched tracked files)
  let plugList  = listPlugins pcfg ls
      fileCache = buildFileCache tracked plugins

  -- Profiles
  let profList = listProfiles prf ls

  -- Coverage stats
  let (assigned, _) = checkCoverage tracked plugins

  pure State
    { _stEnv           = env
    , _stPluginConfig  = pcfg
    , _stProfileConfig = prf
    , _stLocalState    = ls
    , _stAllTracked    = tracked
    , _stTab           = DotfilesTab
    , _stFocus         = FTracked
    , _stPopup         = Nothing
    , _stTrackedList   = L.list RTrackedList (V.fromList groupedList) 1
    , _stUntrackedList = L.list RUntrackedList (V.fromList untrkList) 1
    , _stCollapsed     = collapsed
    , _stSelected      = Set.empty
    , _stFrozen        = frozenSet
    , _stStagedPairs   = stagedStatuses
    , _stUnstagedPairs = unstagedStatuses
    , _stPluginListW   = L.list RPluginList (V.fromList plugList) 1
    , _stPluginFiles   = fileCache
    , _stProfileListW  = L.list RProfileList (V.fromList profList) 1
    , _stSaveItems     = L.list RSaveList V.empty 1
    , _stCommitEditor  = E.editor RCommitEditor Nothing ""
    , _stAssignFiles   = []
    , _stAssignList    = L.list RAssignList V.empty 1
    , _stAssignEditing = False
    , _stAssignEditor  = E.editor RAssignEditor (Just 1) ""
    , _stIgnoreList    = L.list RIgnoreList V.empty 1
    , _stAiMenuList       = L.list RAiMenuList V.empty 1
    , _stPkgItems         = L.list RPkgList V.empty 1
    , _stPkgDistro        = dist
    , _stDetailAdvanced   = False
    , _stNewPluginName    = E.editor RNewPluginName (Just 1) ""
    , _stNewPluginDesc    = E.editor RNewPluginDesc (Just 1) ""
    , _stNewProfileName    = E.editor RNewProfileName (Just 1) ""
    , _stNewProfilePlugins = L.list RNewProfilePlugins V.empty 1
    , _stFilterEditor  = E.editor RFilterEditor (Just 1) ""
    , _stFilterActive  = False
    , _stConfirm       = Nothing
    , _stUncommitted   = uncommitted
    , _stAhead         = ah
    , _stBehind        = bh
    , _stAssignedCount = length assigned
    , _stTotalCount    = length tracked
    , _stError         = initError
    }

-- | Full sync after mutations.
syncAll :: State -> IO State
syncAll st = do
  let env = st ^. stEnv
  st' <- buildState env
  pure $ st'
    & stTab             .~ (st ^. stTab)
    & stFocus           .~ (st ^. stFocus)
    & stPopup           .~ (st ^. stPopup)
    & stCollapsed       .~ (st ^. stCollapsed)
    & stSelected        .~ (st ^. stSelected)
    & stDetailAdvanced  .~ (st ^. stDetailAdvanced)

-- | Sync dotfiles tab data.
syncDotfiles :: State -> IO State
syncDotfiles st = do
  let env  = st ^. stEnv
      pcfg = st ^. stPluginConfig

  ((trackedE, stagedE), (unstagedE, untrkE), (ah, bh)) <-
    concurrently3
      (concurrently (gitTracked env) (gitTrackedStaged env))
      (concurrently (gitTrackedUnstaged env) (gitUntracked env (scopePaths pcfg)))
      (concurrently (gitAhead env) (gitBehind env))

  let tracked  = either (const []) id trackedE
      staged   = either (const []) id stagedE
      unstaged = either (const []) id unstagedE
      untrk    = either (const []) id untrkE
      uncommitted = length staged + length unstaged
      plugins  = _pcPlugins pcfg
      filt     = if st ^. stFilterActive
                 then Just (T.pack $ concat $ E.getEditContents (st ^. stFilterEditor))
                 else Nothing

  stagedStatuses <- mapM (checkStatus env) staged
  unstagedStatuses <- mapM (checkStatus env) unstaged

  frozenE <- listFrozen env
  let frozenSet = Set.fromList $ either (const []) id frozenE

  let groupedList = buildGroupedList plugins tracked stagedStatuses unstagedStatuses (st ^. stCollapsed) filt
      untrkList   = buildUntrackedList plugins (_wlPaths $ _pcWatchlist pcfg) untrk
      (assigned, _) = checkCoverage tracked plugins

  pure $ st
    & stAllTracked    .~ tracked
    & stStagedPairs   .~ stagedStatuses
    & stUnstagedPairs .~ unstagedStatuses
    & stTrackedList   .~ L.list RTrackedList (V.fromList groupedList) 1
    & stUntrackedList .~ L.list RUntrackedList (V.fromList untrkList) 1
    & stFrozen        .~ frozenSet
    & stAssignedCount .~ length assigned
    & stTotalCount    .~ length tracked
    & stUncommitted   .~ uncommitted
    & stAhead         .~ ah
    & stBehind        .~ bh

-- | Sync plugins tab data.
syncPlugins :: State -> IO State
syncPlugins st = do
  let env = st ^. stEnv
  pcfgE <- loadPluginConfig env
  lsE   <- loadLocalState env
  let pcfg    = either (const defaultPluginConfig) id pcfgE
      ls      = either (const (st ^. stLocalState)) id lsE
      plugList = listPlugins pcfg ls
      errs    = either (\e -> [displayError e]) (const []) pcfgE
             ++ either (\e -> [displayError e]) (const []) lsE
      mErr    = if null errs then Nothing else Just errs
  trackedE2 <- gitTracked env
  let tracked2  = either (const []) id trackedE2
      fileCache = buildFileCache tracked2 (_pcPlugins pcfg)
  pure $ st
    & stPluginConfig .~ pcfg
    & stLocalState   .~ ls
    & stPluginListW  .~ L.list RPluginList (V.fromList plugList) 1
    & stPluginFiles  .~ fileCache
    & stError        .~ mErr

-- | Sync profiles tab data.
syncProfiles :: State -> IO State
syncProfiles st = do
  let env = st ^. stEnv
  prfE <- loadProfileConfig env
  lsE  <- loadLocalState env
  let prf      = either (const defaultProfileConfig) id prfE
      ls       = either (const (st ^. stLocalState)) id lsE
      profList = listProfiles prf ls
      errs     = either (\e -> [displayError e]) (const []) prfE
              ++ either (\e -> [displayError e]) (const []) lsE
      mErr     = if null errs then Nothing else Just errs
  pure $ st
    & stProfileConfig .~ prf
    & stLocalState    .~ ls
    & stProfileListW  .~ L.list RProfileList (V.fromList profList) 1
    & stError         .~ mErr

-------------------
-- List builders --
-------------------

-- | Build grouped list of tracked files organized by plugin.
buildGroupedList :: Map.Map PluginName Plugin
                 -> [RelPath]          -- ^ all tracked
                 -> [(RelPath, FileStatus)]  -- ^ staged + status
                 -> [(RelPath, FileStatus)]  -- ^ unstaged + status
                 -> Set PluginName     -- ^ collapsed plugins
                 -> Maybe Text         -- ^ filter text
                 -> [GroupItem]
buildGroupedList plugins tracked stagedPairs unstagedPairs collapsed mFilter =
  let
    matchFilter fp = case mFilter of
      Nothing -> True
      Just f  -> T.toLower f `T.isInfixOf` T.toLower (T.pack fp)

    stagedSet   = Set.fromList (map fst stagedPairs)
    unstagedSet = Set.fromList (map fst unstagedPairs)

    -- Group files by plugin
    pluginGroups = Map.mapWithKey (\_ p -> filter (matchesPlugin p) tracked) plugins
    isManagedPath f = any (\m -> m `isSubpathOf` f) managedPaths
    unassigned = filter (\f -> not (isManagedPath f) && not (any (\p -> matchesPlugin p f) (Map.elems plugins)) && matchFilter f) tracked

    matchesPlugin p fp = any (`isSubpathOf` fp) (_pluginPaths p)

    mkFileItem fp pname
      | fp `Set.member` stagedSet   = GStaged fp pname
      | fp `Set.member` unstagedSet =
          let status = maybe Exists id $ lookup fp unstagedPairs
          in GUnstaged fp status pname
      | otherwise           = GTracked fp pname

    mkPluginGroup pname files =
      let isCollapsed = Set.member pname collapsed
          filteredFiles = filter matchFilter files
          dirtyCount = length $ filter (\f -> f `Set.member` stagedSet || f `Set.member` unstagedSet) filteredFiles
          header = GHeader pname isCollapsed dirtyCount
          items  = if isCollapsed then [] else map (\f -> mkFileItem f pname) filteredFiles
      in if null filteredFiles && not isCollapsed
         then []
         else header : items

    pluginItems = concatMap (uncurry mkPluginGroup) (Map.toAscList pluginGroups)
    unassignedItems = if null unassigned then []
                      else GUnassignedHeader : map GUnassignedFile unassigned
  in pluginItems ++ unassignedItems

-- | Rebuild grouped list preserving selection by plugin name.
rebuildGroupedList :: Maybe PluginName -> State -> State
rebuildGroupedList mName st =
  let pcfg     = st ^. stPluginConfig
      plugins  = _pcPlugins pcfg
      tracked  = st ^. stAllTracked
      filt     = if st ^. stFilterActive
                 then Just (T.pack $ concat $ E.getEditContents (st ^. stFilterEditor))
                 else Nothing
      items    = buildGroupedList plugins tracked (st ^. stStagedPairs) (st ^. stUnstagedPairs) (st ^. stCollapsed) filt
      vec      = V.fromList items
      idx      = case mName of
        Nothing -> Nothing
        Just n  -> V.findIndex (isHeader n) vec
      newList  = L.list RTrackedList vec 1
  in st & stTrackedList .~ maybe newList (\i -> L.listMoveTo i newList) idx
  where
    isHeader n (GHeader name _ _) = name == n
    isHeader _ _                  = False

-- | Build untracked file list.
buildUntrackedList :: Map.Map PluginName Plugin
                   -> [RelPath]     -- ^ watchlist paths
                   -> [RelPath]     -- ^ untracked files
                   -> [UntrackedItem]
buildUntrackedList plugins watchPaths files =
  let (plugScoped, wl) = classifyUntracked files plugins watchPaths
      plugItems = concatMap mkPluginGroup (Map.toAscList plugScoped)
      wlItems   = if null wl then []
                  else UWatchlistHeader : map UWatchlistFile wl
  in plugItems ++ wlItems
  where
    mkPluginGroup (pname, fs) =
      UPluginHeader pname : map (\f -> UPluginFile f pname) fs

--------------------
-- Popup helpers  --
--------------------

-- | Open save popup with current staged/unstaged files.
openSavePopup :: State -> IO State
openSavePopup st = do
  let env = st ^. stEnv
  stagedE   <- gitTrackedStaged env
  unstagedE <- gitTrackedUnstaged env
  let staged   = either (const []) id stagedE
      unstaged = either (const []) id unstagedE
      items = map (\f -> SaveItem f True True) staged
           ++ map (\f -> SaveItem f False False) unstaged
  pure $ st
    & stSaveItems    .~ L.list RSaveList (V.fromList items) 1
    & stCommitEditor .~ E.editor RCommitEditor Nothing ""
    & stPopup        .~ Just SavePopup
    & stFocus        .~ FSaveList

-- | Open assign popup for the given file(s).
openAssignPopup :: [RelPath] -> State -> State
openAssignPopup fps st =
  let pluginMap = _pcPlugins (st ^. stPluginConfig)
      plugins = Map.toAscList pluginMap
      owners = Set.fromList [ n | fp <- fps, Just n <- [findMatchingPlugin fp pluginMap] ]
      items = map (\(n, _) -> (n, Set.member n owners)) plugins
  in st
    & stAssignFiles   .~ fps
    & stAssignList    .~ L.list RAssignList (V.fromList items) 1
    & stAssignEditing .~ False
    & stAssignEditor  .~ E.editor RAssignEditor (Just 1) ""
    & stPopup         .~ Just AssignPopup
    & stFocus         .~ FAssignList

-- | Open ignore popup with path-level segments for selection.
openIgnorePopup :: Maybe FilePath -> State -> State
openIgnorePopup mPath st =
  let segments = pathSegments (fromMaybe "" mPath)
  in st
    & stIgnoreList .~ L.list RIgnoreList (V.fromList segments) 1
    & stPopup      .~ Just IgnorePopup
    & stFocus      .~ FIgnoreList

-- | Open new plugin popup with empty editors.
openNewPluginPopup :: State -> State
openNewPluginPopup st = st
  & stNewPluginName .~ E.editor RNewPluginName (Just 1) ""
  & stNewPluginDesc .~ E.editor RNewPluginDesc (Just 1) ""
  & stPopup         .~ Just NewPluginPopup
  & stFocus         .~ FNewPluginName

-- | Open new profile popup with plugin toggle list.
openNewProfilePopup :: State -> State
openNewProfilePopup st =
  let plugins = Map.toAscList $ _pcPlugins (st ^. stPluginConfig)
      items   = map (\(n, _) -> (n, False)) plugins
  in st
    & stNewProfileName    .~ E.editor RNewProfileName (Just 1) ""
    & stNewProfilePlugins .~ L.list RNewProfilePlugins (V.fromList items) 1
    & stPopup             .~ Just NewProfilePopup
    & stFocus             .~ FNewProfileName

-- | Open package popup with missing packages (all selected by default).
openPackagePopup :: Distro -> [Text] -> [Text] -> State -> State
openPackagePopup dist missing caskPkgs st =
  let items = map (\pkg -> PkgItem pkg True (pkg `elem` caskPkgs)) missing
  in st
    & stPkgItems  .~ L.list RPkgList (V.fromList items) 1
    & stPkgDistro .~ dist
    & stPopup     .~ Just PackagePopup
    & stFocus     .~ FPkgList

-- | Split a path into cumulative prefix segments.
-- e.g. ".some/dir/file.txt" -> [".some", ".some/dir", ".some/dir/file.txt"]
pathSegments :: FilePath -> [FilePath]
pathSegments "" = []
pathSegments fp =
  let parts = splitOn '/' fp
      prefixes = scanl1 (\a b -> a ++ "/" ++ b) parts
  in prefixes
  where
    splitOn _ [] = []
    splitOn c s  = let (w, rest) = break (== c) s
                   in w : case rest of
                            []    -> []
                            (_:r) -> splitOn c r

-----------
-- Utils --
-----------

-- | Check if a file exists (for FileStatus).
checkStatus :: GitEnv -> RelPath -> IO (RelPath, FileStatus)
checkStatus env fp = do
  exists <- doesPathExist (_geHome env </> fp)
  pure (fp, if exists then Exists else Deleted)

-- | Build file cache: map plugin name -> list of tracked files.
-- Accepts already-fetched tracked files to avoid redundant git calls.
buildFileCache :: [RelPath] -> Map.Map PluginName Plugin -> Map.Map PluginName [RelPath]
buildFileCache tracked plugins =
  Map.map (\p -> filter (\fp -> any (`isSubpathOf` fp) (_pluginPaths p)) tracked) plugins

-- | Run three IO actions concurrently.
concurrently3 :: IO a -> IO b -> IO c -> IO (a, b, c)
concurrently3 a b c = do
  (x, (y, z)) <- concurrently a (concurrently b c)
  pure (x, y, z)
