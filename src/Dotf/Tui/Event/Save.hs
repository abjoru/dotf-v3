module Dotf.Tui.Event.Save (
  handleSaveEvent,
) where

import           Brick                  (BrickEvent (..))
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.Edit     as E
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Vector            as V
import           Dotf.Git               (gitCommit, gitPush, gitStage,
                                         gitUnstage)
import           Dotf.Tui.Types
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (use, zoom, (.=))

-- | Handle save popup events.
handleSaveEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Esc: cancel
handleSaveEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  stPopup .= Nothing
  stFocus .= FTracked

-- Tab: cycle focus between file list and editor
handleSaveEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  f <- use stFocus
  case f of
    FSaveList   -> stFocus .= FSaveEditor
    FSaveEditor -> stFocus .= FSaveList
    _           -> pure ()

-- Space: toggle file selection (in list focus)
handleSaveEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  f <- use stFocus
  case f of
    FSaveList -> do
      sl <- use stSaveItems
      case L.listSelectedElement sl of
        Nothing -> pure ()
        Just (idx, item) -> do
          let toggled = item { _siSelected = not (_siSelected item) }
              items = L.listElements sl
              items' = items V.// [(idx, toggled)]
          stSaveItems .= L.list RSaveList items' 1
    _ -> pure ()

-- j/k navigation in list
handleSaveEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = navSave (V.EvKey V.KDown [])
handleSaveEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = navSave (V.EvKey V.KUp [])
handleSaveEvent (VtyEvent ev@(V.EvKey V.KDown []))     = navSave ev
handleSaveEvent (VtyEvent ev@(V.EvKey V.KUp []))       = navSave ev

-- Enter: commit + push
handleSaveEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  f <- use stFocus
  case f of
    FSaveEditor -> doCommitAndPush
    FSaveList   -> doCommitAndPush
    _           -> pure ()

-- Editor events
handleSaveEvent (VtyEvent ev) = do
  f <- use stFocus
  case f of
    FSaveEditor -> zoom stCommitEditor $ E.handleEditorEvent (VtyEvent ev)
    _           -> pure ()

handleSaveEvent _ = pure ()

-- | Navigate save list.
navSave :: V.Event -> EventM RName State ()
navSave ev = do
  f <- use stFocus
  case f of
    FSaveList -> zoom stSaveItems $ L.handleListEvent ev
    _         -> pure ()

-- | Commit selected files and push.
doCommitAndPush :: EventM RName State ()
doCommitAndPush = do
  st <- get
  let env     = st ^. stEnv
      items   = V.toList $ L.listElements (st ^. stSaveItems)
      selected = filter _siSelected items
      msg     = unlines $ E.getEditContents (st ^. stCommitEditor)
  if null selected || null (filter (not . null) (lines msg))
    then stError .= Just ["No files selected or empty commit message"]
    else do
      -- Stage selected, unstage unselected
      result <- liftIO $ do
        mapM_ (\i ->
          if _siSelected i
            then gitStage env (_siPath i) >> pure ()
            else gitUnstage env (_siPath i) >> pure ()
          ) items
        commitResult <- gitCommit env (strip msg)
        case commitResult of
          Left err -> pure $ Left err
          Right () -> gitPush env
      case result of
        Left err -> stError .= Just [show err]
        Right () -> do
          stPopup .= Nothing
          stFocus .= FTracked
          st' <- liftIO $ syncDotfiles st
          put st'
  where
    strip = reverse . dropWhile (== '\n') . reverse . dropWhile (== '\n')
