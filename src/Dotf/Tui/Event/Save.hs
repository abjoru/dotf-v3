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
import           Dotf.Types             (displayError)
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

-- Space: toggle file selection (list focus only)
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
          stSaveItems .= L.listMoveTo idx (L.list RSaveList items' 1)
    FSaveEditor -> zoom stCommitEditor $ E.handleEditorEvent (VtyEvent (V.EvKey (V.KChar ' ') []))
    _ -> pure ()

-- Enter: commit + push
handleSaveEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  f <- use stFocus
  case f of
    FSaveEditor -> doCommitAndPush
    FSaveList   -> doCommitAndPush
    _           -> pure ()

-- Fallback: route to focused widget
handleSaveEvent (VtyEvent ev) = do
  f <- use stFocus
  case f of
    FSaveEditor -> zoom stCommitEditor $ E.handleEditorEvent (VtyEvent ev)
    FSaveList   -> zoom stSaveItems $ L.handleListEventVi L.handleListEvent ev
    _           -> pure ()

handleSaveEvent _ = pure ()

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
      -- Stage selected, unstage unselected; skip individual failures
      result <- liftIO $ do
        -- Stage/unstage, collecting per-file results
        -- Only change staging state for files whose selection differs from original
        mapM_ (\i -> case (_siSelected i, _siIsStaged i) of
          (True,  False) -> gitStage env (_siPath i)    -- user checked unstaged file
          (False, True)  -> gitUnstage env (_siPath i)  -- user unchecked staged file
          _              -> pure (Right ())             -- no change needed
          ) items
        -- Commit whatever is staged
        commitResult <- gitCommit env (strip msg)
        case commitResult of
          Left err -> pure $ Left err
          Right () -> gitPush env
      case result of
        Left err -> stError .= Just [displayError err]
        Right () -> do
          stPopup .= Nothing
          stFocus .= FTracked
          st' <- get
          st'' <- liftIO $ syncDotfiles st'
          put st''
  where
    strip = reverse . dropWhile (== '\n') . reverse . dropWhile (== '\n')
