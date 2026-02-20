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

  -- * Lenses
  stEnv, stPluginConfig, stProfileConfig, stLocalState, stAllTracked,
  stTab, stFocus, stPopup,
  stTrackedList, stUntrackedList, stCollapsed, stSelected,
  stPluginListW, stPluginFiles,
  stProfileListW,
  stSaveItems, stCommitEditor,
  stAssignFiles, stAssignList, stAssignEditing, stAssignEditor,
  stIgnoreEditor,
  stFilterEditor, stFilterActive,
  stConfirm,
  stAhead, stBehind, stAssignedCount, stTotalCount,
  stError,
  siPath, siSelected, siIsStaged,
) where

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Data.Map.Strict    as Map
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Vector        as V
import           Dotf.Config
import           Dotf.Git
import           Dotf.Plugin        (listPlugins)
import           Dotf.Profile       (checkCoverage, listProfiles)
import           Dotf.State
import           Dotf.Tracking      (classifyUntracked)
import           Dotf.Types
import           Lens.Micro         ((&), (.~), (^.))
import           Lens.Micro.TH      (makeLenses)
import           System.Directory   (doesPathExist)
import           System.FilePath    ((</>))

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
  | FIgnoreEditor
  | FFilterEditor
  deriving (Eq, Show, Ord)

data Popup = SavePopup | AssignPopup | IgnorePopup | FilterPopup
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
  | RIgnoreEditor
  | RFilterEditor
  | RPluginDetail
  | RProfileDetail
  deriving (Eq, Show, Ord)

-- | Grouped item in tracked list: headers or file entries.
data GroupItem
  = GHeader PluginName Bool  -- ^ plugin name, collapsed?
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

-- | Custom event type for Brick.
data DEvent = Tick
  deriving (Eq, Show)

-- | Confirmable actions.
data ConfirmAction
  = ConfirmUntrack RelPath
  | ConfirmDeletePlugin PluginName
  | ConfirmRemovePlugin PluginName
  | ConfirmDeleteProfile ProfileName
  | ConfirmDeactivateProfile
  deriving (Eq, Show)

-----------
-- State --
-----------

data State = State
  { _stEnv           :: GitEnv
  , _stPluginConfig  :: PluginConfig
  , _stProfileConfig :: ProfileConfig
  , _stLocalState    :: LocalState
  , _stAllTracked    :: [RelPath]

  -- Navigation
  , _stTab           :: Tab
  , _stFocus         :: Focus
  , _stPopup         :: Maybe Popup

  -- Dotfiles tab
  , _stTrackedList   :: L.List RName GroupItem
  , _stUntrackedList :: L.List RName UntrackedItem
  , _stCollapsed     :: Set PluginName
  , _stSelected      :: Set RelPath

  -- Plugins tab
  , _stPluginListW   :: L.List RName (Plugin, Bool)
  , _stPluginFiles   :: Map.Map PluginName [RelPath]

  -- Profiles tab
  , _stProfileListW  :: L.List RName (Profile, Bool)

  -- Save popup
  , _stSaveItems     :: L.List RName SaveItem
  , _stCommitEditor  :: E.Editor String RName

  -- Assign popup
  , _stAssignFiles   :: [RelPath]
  , _stAssignList    :: L.List RName (PluginName, Bool)
  , _stAssignEditing :: Bool
  , _stAssignEditor  :: E.Editor String RName

  -- Ignore popup
  , _stIgnoreEditor  :: E.Editor String RName

  -- Filter
  , _stFilterEditor  :: E.Editor String RName
  , _stFilterActive  :: Bool

  -- Confirm dialog
  , _stConfirm       :: Maybe (String, ConfirmAction)

  -- Status bar
  , _stAhead         :: Int
  , _stBehind        :: Int
  , _stAssignedCount :: Int
  , _stTotalCount    :: Int

  -- Error
  , _stError         :: Maybe [String]
  }

makeLenses ''SaveItem
makeLenses ''State

--------------------
-- State builders --
--------------------

-- | Build initial TUI state from a GitEnv.
buildState :: GitEnv -> IO State
buildState env = do
  pcfgE  <- loadPluginConfig env
  prfE   <- loadProfileConfig env
  ls     <- loadLocalState env
  (ah, bh) <- gitAheadBehind env

  let pcfg = either (const defaultPluginConfig) id pcfgE
      prf  = either (const defaultProfileConfig) id prfE

  -- Tracked files
  trackedE  <- gitTracked env
  stagedE   <- gitTrackedStaged env
  unstagedE <- gitTrackedUnstaged env
  untrkE    <- gitUntracked env

  let tracked  = either (const []) id trackedE
      staged   = either (const []) id stagedE
      unstaged = either (const []) id unstagedE
      untrk    = either (const []) id untrkE

  -- Classify tracked files with status
  stagedStatuses <- mapM (checkStatus env) staged
  unstagedStatuses <- mapM (checkStatus env) unstaged

  let plugins     = _pcPlugins pcfg
      collapsed   = Set.fromList (Map.keys plugins)
      groupedList = buildGroupedList plugins tracked stagedStatuses unstagedStatuses collapsed Nothing
      untrkList   = buildUntrackedList plugins (_wlPaths $ _pcWatchlist pcfg) untrk

  -- Plugin files cache
  let plugList    = listPlugins pcfg ls
  fileCache <- buildFileCache env plugins

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
    , _stPluginListW   = L.list RPluginList (V.fromList plugList) 1
    , _stPluginFiles   = fileCache
    , _stProfileListW  = L.list RProfileList (V.fromList profList) 1
    , _stSaveItems     = L.list RSaveList V.empty 1
    , _stCommitEditor  = E.editor RCommitEditor Nothing ""
    , _stAssignFiles   = []
    , _stAssignList    = L.list RAssignList V.empty 1
    , _stAssignEditing = False
    , _stAssignEditor  = E.editor RAssignEditor (Just 1) ""
    , _stIgnoreEditor  = E.editor RIgnoreEditor (Just 1) ""
    , _stFilterEditor  = E.editor RFilterEditor (Just 1) ""
    , _stFilterActive  = False
    , _stConfirm       = Nothing
    , _stAhead         = ah
    , _stBehind        = bh
    , _stAssignedCount = length assigned
    , _stTotalCount    = length tracked
    , _stError         = Nothing
    }

-- | Full sync after mutations.
syncAll :: State -> IO State
syncAll st = do
  let env = st ^. stEnv
  st' <- buildState env
  pure $ st'
    & stTab       .~ (st ^. stTab)
    & stFocus     .~ (st ^. stFocus)
    & stPopup     .~ (st ^. stPopup)
    & stCollapsed .~ (st ^. stCollapsed)
    & stSelected  .~ (st ^. stSelected)

-- | Sync dotfiles tab data.
syncDotfiles :: State -> IO State
syncDotfiles st = do
  let env  = st ^. stEnv
      pcfg = st ^. stPluginConfig

  trackedE  <- gitTracked env
  stagedE   <- gitTrackedStaged env
  unstagedE <- gitTrackedUnstaged env
  untrkE    <- gitUntracked env

  let tracked  = either (const []) id trackedE
      staged   = either (const []) id stagedE
      unstaged = either (const []) id unstagedE
      untrk    = either (const []) id untrkE
      plugins  = _pcPlugins pcfg
      filt     = if st ^. stFilterActive
                 then Just (T.pack $ concat $ E.getEditContents (st ^. stFilterEditor))
                 else Nothing

  stagedStatuses <- mapM (checkStatus env) staged
  unstagedStatuses <- mapM (checkStatus env) unstaged

  let groupedList = buildGroupedList plugins tracked stagedStatuses unstagedStatuses (st ^. stCollapsed) filt
      untrkList   = buildUntrackedList plugins (_wlPaths $ _pcWatchlist pcfg) untrk
      (assigned, _) = checkCoverage tracked plugins

  (ah, bh) <- gitAheadBehind env

  pure $ st
    & stAllTracked    .~ tracked
    & stTrackedList   .~ L.list RTrackedList (V.fromList groupedList) 1
    & stUntrackedList .~ L.list RUntrackedList (V.fromList untrkList) 1
    & stAssignedCount .~ length assigned
    & stTotalCount    .~ length tracked
    & stAhead         .~ ah
    & stBehind        .~ bh

-- | Sync plugins tab data.
syncPlugins :: State -> IO State
syncPlugins st = do
  let env = st ^. stEnv
  pcfgE <- loadPluginConfig env
  ls    <- loadLocalState env
  let pcfg = either (const defaultPluginConfig) id pcfgE
      plugList = listPlugins pcfg ls
  fileCache <- buildFileCache env (_pcPlugins pcfg)
  pure $ st
    & stPluginConfig .~ pcfg
    & stLocalState   .~ ls
    & stPluginListW  .~ L.list RPluginList (V.fromList plugList) 1
    & stPluginFiles  .~ fileCache

-- | Sync profiles tab data.
syncProfiles :: State -> IO State
syncProfiles st = do
  let env = st ^. stEnv
  prfE <- loadProfileConfig env
  ls   <- loadLocalState env
  let prf     = either (const defaultProfileConfig) id prfE
      profList = listProfiles prf ls
  pure $ st
    & stProfileConfig .~ prf
    & stLocalState    .~ ls
    & stProfileListW  .~ L.list RProfileList (V.fromList profList) 1

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

    staged   = map fst stagedPairs
    unstaged = map fst unstagedPairs

    -- Group files by plugin
    pluginGroups = Map.mapWithKey (\_ p -> filter (matchesPlugin p) tracked) plugins
    unassigned = filter (\f -> not (any (\p -> matchesPlugin p f) (Map.elems plugins)) && matchFilter f) tracked

    matchesPlugin p fp =
      any (\pp -> pp `isPrefixOf'` fp || fp == pp) (_pluginPaths p)

    isPrefixOf' prefix path =
      let prefix' = if null prefix || last prefix == '/' then prefix
                    else prefix ++ "/"
      in take (length prefix') path == prefix'

    mkFileItem fp pname
      | fp `elem` staged   = GStaged fp pname
      | fp `elem` unstaged =
          let status = maybe Exists snd $ lookup' fp unstagedPairs
          in GUnstaged fp status pname
      | otherwise           = GTracked fp pname
      where
        lookup' k = foldr (\(a,b) acc -> if a == k then Just (a,b) else acc) Nothing

    mkPluginGroup pname files =
      let isCollapsed = Set.member pname collapsed
          filteredFiles = filter matchFilter files
          header = GHeader pname isCollapsed
          items  = if isCollapsed then [] else map (\f -> mkFileItem f pname) filteredFiles
      in if null filteredFiles && not isCollapsed
         then []
         else header : items

    pluginItems = concatMap (uncurry mkPluginGroup) (Map.toAscList pluginGroups)
    unassignedItems = if null unassigned then []
                      else GUnassignedHeader : map GUnassignedFile unassigned
  in pluginItems ++ unassignedItems

-- | Rebuild grouped list preserving selection.
rebuildGroupedList :: State -> State
rebuildGroupedList st =
  let pcfg     = st ^. stPluginConfig
      plugins  = _pcPlugins pcfg
      tracked  = st ^. stAllTracked
      filt     = if st ^. stFilterActive
                 then Just (T.pack $ concat $ E.getEditContents (st ^. stFilterEditor))
                 else Nothing
      -- We don't have staged/unstaged info cached, so rebuild from scratch
      items    = buildGroupedList plugins tracked [] [] (st ^. stCollapsed) filt
  in st & stTrackedList .~ L.list RTrackedList (V.fromList items) 1

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
  let plugins = Map.toAscList $ _pcPlugins (st ^. stPluginConfig)
      items = map (\(n, _) -> (n, False)) plugins
  in st
    & stAssignFiles   .~ fps
    & stAssignList    .~ L.list RAssignList (V.fromList items) 1
    & stAssignEditing .~ False
    & stAssignEditor  .~ E.editor RAssignEditor (Just 1) ""
    & stPopup         .~ Just AssignPopup
    & stFocus         .~ FAssignList

-- | Open ignore popup.
openIgnorePopup :: State -> State
openIgnorePopup st = st
  & stIgnoreEditor .~ E.editor RIgnoreEditor (Just 1) ""
  & stPopup        .~ Just IgnorePopup
  & stFocus        .~ FIgnoreEditor

-----------
-- Utils --
-----------

-- | Check if a file exists (for FileStatus).
checkStatus :: GitEnv -> RelPath -> IO (RelPath, FileStatus)
checkStatus env fp = do
  exists <- doesPathExist (_geHome env </> fp)
  pure (fp, if exists then Exists else Deleted)

-- | Build file cache: map plugin name -> list of tracked files.
buildFileCache :: GitEnv -> Map.Map PluginName Plugin -> IO (Map.Map PluginName [RelPath])
buildFileCache env plugins = do
  trackedE <- gitTracked env
  let tracked = either (const []) id trackedE
  pure $ Map.mapWithKey (\_ p ->
    filter (\fp -> any (\pp -> pp `isPrefixOf'` fp || fp == pp) (_pluginPaths p)) tracked
    ) plugins
  where
    isPrefixOf' prefix path =
      let prefix' = if null prefix || last prefix == '/' then prefix
                    else prefix ++ "/"
      in take (length prefix') path == prefix'
