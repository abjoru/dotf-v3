module Dotf.Tui.Event.Dotfiles (
  handleDotfilesEvent,
) where

import           Brick                  (BrickEvent (..), suspendAndResume)
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Set               as Set
import           Dotf.Git               (gitDiffFile)
import           Dotf.Tui.Types
import           Dotf.Types             (GitEnv (..))
import           Dotf.Utils             (editFile)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (use, zoom, (.=))

-- | Handle events in Dotfiles tab.
handleDotfilesEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Navigation
handleDotfilesEvent (VtyEvent ev@(V.EvKey V.KDown []))      = handleListNav ev
handleDotfilesEvent (VtyEvent ev@(V.EvKey V.KUp []))        = handleListNav ev
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = handleListNav (V.EvKey V.KDown [])
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = handleListNav (V.EvKey V.KUp [])
handleDotfilesEvent (VtyEvent ev@(V.EvKey V.KHome []))   = handleListNav ev
handleDotfilesEvent (VtyEvent ev@(V.EvKey V.KEnd []))       = handleListNav ev

-- Space: toggle collapse on headers
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = toggleCollapse

-- e: edit selected file
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'e') [])) = editSelected

-- d: diff selected file
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = diffSelected

-- u: untrack (triggers confirm)
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'u') [])) = do
  mPath <- getSelectedPath
  case mPath of
    Nothing -> pure ()
    Just fp -> stConfirm .= Just ("Untrack " ++ fp ++ "?", ConfirmUntrack fp)

-- a: assign trigger
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
  mPath <- getSelectedPath
  case mPath of
    Nothing -> pure ()
    Just fp -> do
      st <- get
      put $ openAssignPopup fp st

-- s: save trigger
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 's') [])) = do
  st <- get
  st' <- liftIO $ openSavePopup st
  put st'

-- I: ignore trigger
handleDotfilesEvent (VtyEvent (V.EvKey (V.KChar 'I') [])) = do
  st <- get
  put $ openIgnorePopup st

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

handleDotfilesEvent _ = pure ()

-- | Navigate list based on current focus.
handleListNav :: V.Event -> EventM RName State ()
handleListNav ev = do
  f <- use stFocus
  case f of
    FTracked   -> zoom stTrackedList $ L.handleListEvent ev
    FUntracked -> zoom stUntrackedList $ L.handleListEvent ev
    _          -> pure ()

-- | Toggle collapse on plugin header.
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
          put $ rebuildGroupedList st
        _ -> pure ()
    _ -> pure ()

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
          Left _     -> pure ()
          Right diff' -> do
            putStrLn diff'
            putStrLn "\nPress Enter to continue..."
            _ <- getLine
            pure ()
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
