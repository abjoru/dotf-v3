module Dotf.Tui.Event.Assign (
  handleAssignEvent,
) where

import           Brick                  (BrickEvent (..))
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.Edit     as E
import qualified Brick.Widgets.List     as L
import           Control.Monad          (foldM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Set               as Set
import qualified Data.Text              as T
import           Dotf.Plugin            (createPlugin)
import           Dotf.Tracking          (trackFile)
import           Dotf.Tui.Types
import           Dotf.Types             (DotfError, GitEnv, PluginName, RelPath,
                                         displayError)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (use, zoom, (.=))

-- | Handle assign popup events.
handleAssignEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Esc: cancel
handleAssignEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  editing <- use stAssignEditing
  if editing
    then stAssignEditing .= False
    else do
      stPopup .= Nothing
      stFocus .= FTracked

-- +: start inline editor for new plugin name
handleAssignEvent (VtyEvent (V.EvKey (V.KChar '+') [])) = do
  editing <- use stAssignEditing
  if not editing
    then do
      stAssignEditing .= True
      stAssignEditor .= E.editor RAssignEditor (Just 1) ""
      stFocus .= FAssignEditor
    else pure ()

-- Enter: assign to selected plugin or create new
handleAssignEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  editing <- use stAssignEditing
  if editing
    then doCreateAndAssign
    else doAssign

-- j/k: vim nav when not editing
handleAssignEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = viNav V.KDown
handleAssignEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = viNav V.KUp

-- Fallback: editor when editing, list nav otherwise
handleAssignEvent (VtyEvent ev) = navOrEdit ev
handleAssignEvent _ = pure ()

-- | Vi-style navigation (only when not editing).
viNav :: V.Key -> EventM RName State ()
viNav k = do
  editing <- use stAssignEditing
  if editing
    then zoom stAssignEditor $ E.handleEditorEvent (VtyEvent (V.EvKey k []))
    else zoom stAssignList $ L.handleListEvent (V.EvKey k [])

-- | Route event to list nav or editor depending on editing state.
navOrEdit :: V.Event -> EventM RName State ()
navOrEdit ev = do
  editing <- use stAssignEditing
  if editing
    then zoom stAssignEditor $ E.handleEditorEvent (VtyEvent ev)
    else zoom stAssignList $ L.handleListEvent ev

-- | Assign file(s) to selected existing plugin.
doAssign :: EventM RName State ()
doAssign = do
  st <- get
  let env = st ^. stEnv
      fps = st ^. stAssignFiles
  if null fps
    then pure ()
    else do
      al <- use stAssignList
      case L.listSelectedElement al of
        Nothing -> pure ()
        Just (_, (pluginName, _)) -> do
          errs <- liftIO $ trackFiles env fps (Just pluginName)
          if null errs
            then do
              stPopup .= Nothing
              stFocus .= FTracked
              stSelected .= Set.empty
              st' <- get
              st'' <- liftIO $ syncAll st'
              put st''
            else stError .= Just (map displayError errs)

-- | Create new plugin and assign file(s) to it.
doCreateAndAssign :: EventM RName State ()
doCreateAndAssign = do
  st <- get
  let env  = st ^. stEnv
      name = T.pack $ concat $ E.getEditContents (st ^. stAssignEditor)
      fps  = st ^. stAssignFiles
  if null fps
    then pure ()
    else do
      if T.null (T.strip name)
        then stError .= Just ["Plugin name cannot be empty"]
        else do
          let pname = T.strip name
          createResult <- liftIO $ createPlugin env pname Nothing
          case createResult of
            Left err -> stError .= Just [displayError err]
            Right () -> do
              errs <- liftIO $ trackFiles env fps (Just pname)
              if null errs
                then do
                  stPopup .= Nothing
                  stAssignEditing .= False
                  stFocus .= FTracked
                  stSelected .= Set.empty
                  st' <- get
                  st'' <- liftIO $ syncAll st'
                  put st''
                else stError .= Just (map displayError errs)

-- | Track multiple files, collecting errors.
trackFiles :: GitEnv -> [RelPath] -> Maybe PluginName -> IO [DotfError]
trackFiles env fps mPlug = do
  results <- foldM (\acc fp -> do
    r <- trackFile env fp mPlug
    pure $ case r of
      Left err -> err : acc
      Right () -> acc
    ) [] fps
  pure (reverse results)
