module Dotf.Tui.Event.Dotfiles (
  handleDotfilesEvent,
) where

import           Brick                  (BrickEvent (..), suspendAndResume)
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import           Dotf.Git               (gitDiffFile)
import           Dotf.Path              (isSubpathOf)
import           Dotf.Tui.Types
import           Dotf.Types
import           Dotf.Utils             (editFile)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((&), (.~), (^.))
import           Lens.Micro.Mtl         (use, zoom, (.=))

-- | Handle events in Dotfiles tab.
handleDotfilesEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Space: multi-select toggle
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = toggleSelect

-- Enter: collapse toggle on headers
handleDotfilesEvent (VtyEvent (V.EvKey V.KEnter [])) = toggleCollapse

-- e: edit selected file
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'e') [])) = editSelected

-- d: diff selected file
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = diffSelected

-- u: untrack (triggers confirm, batch if selected)
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'u') [])) = do
  st <- get
  let sel = st ^. stSelected
  if not (Set.null sel)
    then let fps = Set.toList sel
             msg = "Untrack " ++ show (length fps) ++ " files?"
         in stConfirm .= Just (msg, ConfirmUntrack fps)
    else do
      mPath <- getSelectedPath
      case mPath of
        Nothing -> pure ()
        Just fp -> stConfirm .= Just ("Untrack " ++ fp ++ "?", ConfirmUntrack [fp])

-- a: assign trigger (batch if selected, else cursor)
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
  st <- get
  let sel = st ^. stSelected
  if not (Set.null sel)
    then put $ openAssignPopup (Set.toList sel) st
    else do
      mPath <- getSelectedPath
      case mPath of
        Nothing -> pure ()
        Just fp -> put $ openAssignPopup [fp] st

-- s: save trigger
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 's') [])) = do
  st <- get
  st' <- liftIO $ openSavePopup st
  put st'

-- I: ignore trigger (pre-populate with selected file)
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'I') [])) = do
  mPath <- getSelectedPath
  st <- get
  put $ openIgnorePopup mPath st

-- f: open filter
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'f') [])) = do
  stPopup .= Just FilterPopup
  stFocus .= FFilterEditor

-- F: clear filter
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'F') [])) = do
  stFilterActive .= False
  st <- get
  st' <- liftIO $ syncDotfiles st
  put st'

-- Fallback: delegate to list vi navigation
handleDotfilesEvent (VtyEvent ev) = handleListNav ev
handleDotfilesEvent _ = pure ()

-- | Navigate list based on current focus (vi keys enabled).
handleListNav :: V.Event -> EventM RName State ()
handleListNav ev = do
  f <- use stFocus
  case f of
    FTracked   -> zoom stTrackedList $ L.handleListEventVi L.handleListEvent ev
    FUntracked -> zoom stUntrackedList $ L.handleListEventVi L.handleListEvent ev
    _          -> pure ()

-- | Toggle collapse on plugin header (Enter key).
toggleCollapse :: EventM RName State ()
toggleCollapse = do
  f <- use stFocus
  case f of
    FTracked -> do
      tl <- use stTrackedList
      case L.listSelectedElement tl of
        Just (_, GHeader name _) -> do
          collapsed <- use stCollapsed
          let collapsed' = if Set.member name collapsed
                           then Set.delete name collapsed
                           else Set.insert name collapsed
          stCollapsed .= collapsed'
          st <- get
          put $ rebuildGroupedList (Just name) st
        _ -> pure ()
    _ -> pure ()

-- | Toggle multi-select on files (Space key).
toggleSelect :: EventM RName State ()
toggleSelect = do
  f <- use stFocus
  case f of
    FTracked -> do
      tl <- use stTrackedList
      case L.listSelectedElement tl of
        Just (_, item) -> do
          case itemPath item of
            Just fp -> do
              selected <- use stSelected
              let selected' = if Set.member fp selected
                              then Set.delete fp selected
                              else Set.insert fp selected
              stSelected .= selected'
              -- Advance cursor
              zoom stTrackedList $ L.handleListEvent (V.EvKey V.KDown [])
            Nothing -> toggleGroupSelect item
        _ -> pure ()
    _ -> pure ()

-- | Toggle all files in a plugin group.
toggleGroupSelect :: GroupItem -> EventM RName State ()
toggleGroupSelect (GHeader pname _) = do
  st <- get
  let plugins  = _pcPlugins (st ^. stPluginConfig)
      tracked  = st ^. stAllTracked
      selected = st ^. stSelected
  case Map.lookup pname plugins of
    Nothing -> pure ()
    Just p  -> do
      let groupFiles = filter (matchesPlugin p) tracked
          allSelected = all (`Set.member` selected) groupFiles && not (null groupFiles)
          selected' = if allSelected
                      then foldr Set.delete selected groupFiles
                      else foldr Set.insert selected groupFiles
      stSelected .= selected'
toggleGroupSelect GUnassignedHeader = do
  st <- get
  let plugins  = _pcPlugins (st ^. stPluginConfig)
      tracked  = st ^. stAllTracked
      selected = st ^. stSelected
      unassigned = filter (\fp -> not (any (\p -> matchesPlugin p fp) (Map.elems plugins))) tracked
      allSelected = all (`Set.member` selected) unassigned && not (null unassigned)
      selected' = if allSelected
                  then foldr Set.delete selected unassigned
                  else foldr Set.insert selected unassigned
  stSelected .= selected'
toggleGroupSelect _ = pure ()

-- | Check if a file belongs to a plugin.
matchesPlugin :: Plugin -> RelPath -> Bool
matchesPlugin p fp = any (`isSubpathOf` fp) (_pluginPaths p)

-- | Extract file path from a GroupItem, if it's a file item.
itemPath :: GroupItem -> Maybe RelPath
itemPath (GStaged fp _)       = Just fp
itemPath (GUnstaged fp _ _)   = Just fp
itemPath (GTracked fp _)      = Just fp
itemPath (GUnassignedFile fp) = Just fp
itemPath _                    = Nothing

-- | Edit selected file with $EDITOR.
editSelected :: EventM RName State ()
editSelected = do
  mPath <- getSelectedPath
  case mPath of
    Nothing -> pure ()
    Just fp -> do
      st <- get
      let home = _geHome (st ^. stEnv)
      suspendAndResume $ do
        editFile (home ++ "/" ++ fp)
        syncDotfiles st

-- | Diff selected file.
diffSelected :: EventM RName State ()
diffSelected = do
  mPath <- getSelectedPath
  case mPath of
    Nothing -> pure ()
    Just fp -> do
      st <- get
      let env = st ^. stEnv
      suspendAndResume $ do
        result <- gitDiffFile env fp
        case result of
          Left err    -> do
            st' <- syncDotfiles st
            pure $ st' & stError .~ Just [displayError err]
          Right diff' -> do
            putStrLn diff'
            putStrLn "\nPress Enter to continue..."
            _ <- getLine
            syncDotfiles st

-- | Get the file path from the currently selected item.
getSelectedPath :: EventM RName State (Maybe FilePath)
getSelectedPath = do
  f <- use stFocus
  case f of
    FTracked -> do
      tl <- use stTrackedList
      pure $ case L.listSelectedElement tl of
        Just (_, GStaged fp _)       -> Just fp
        Just (_, GUnstaged fp _ _)   -> Just fp
        Just (_, GTracked fp _)      -> Just fp
        Just (_, GUnassignedFile fp) -> Just fp
        _                            -> Nothing
    FUntracked -> do
      ul <- use stUntrackedList
      pure $ case L.listSelectedElement ul of
        Just (_, UPluginFile fp _)  -> Just fp
        Just (_, UWatchlistFile fp) -> Just fp
        _                           -> Nothing
    _ -> pure Nothing
