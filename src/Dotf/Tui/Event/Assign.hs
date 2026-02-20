module Dotf.Tui.Event.Assign (
  handleAssignEvent,
) where

import           Brick                  (BrickEvent (..))
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.Edit     as E
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           Dotf.Tracking          (trackFile)
import           Dotf.Tui.Types
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

-- j/k navigation
handleAssignEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = navAssign (V.EvKey V.KDown [])
handleAssignEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = navAssign (V.EvKey V.KUp [])
handleAssignEvent (VtyEvent ev@(V.EvKey V.KDown []))     = navAssign ev
handleAssignEvent (VtyEvent ev@(V.EvKey V.KUp []))       = navAssign ev

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

-- Editor events when editing
handleAssignEvent (VtyEvent ev) = do
  editing <- use stAssignEditing
  if editing
    then zoom stAssignEditor $ E.handleEditorEvent (VtyEvent ev)
    else pure ()

handleAssignEvent _ = pure ()

-- | Navigate assign list.
navAssign :: V.Event -> EventM RName State ()
navAssign ev = do
  editing <- use stAssignEditing
  if not editing
    then zoom stAssignList $ L.handleListEvent ev
    else pure ()

-- | Assign file to selected existing plugin.
doAssign :: EventM RName State ()
doAssign = do
  st <- get
  let env = st ^. stEnv
  case st ^. stAssignFile of
    Nothing -> pure ()
    Just fp -> do
      al <- use stAssignList
      case L.listSelectedElement al of
        Nothing -> pure ()
        Just (_, (pluginName, _)) -> do
          result <- liftIO $ trackFile env fp (Just pluginName)
          case result of
            Left err -> stError .= Just [show err]
            Right () -> do
              stPopup .= Nothing
              stFocus .= FTracked
              st' <- liftIO $ syncAll st
              put st'

-- | Create new plugin and assign file to it.
doCreateAndAssign :: EventM RName State ()
doCreateAndAssign = do
  st <- get
  let env  = st ^. stEnv
      name = T.pack $ concat $ E.getEditContents (st ^. stAssignEditor)
  case st ^. stAssignFile of
    Nothing -> pure ()
    Just fp -> do
      if T.null (T.strip name)
        then stError .= Just ["Plugin name cannot be empty"]
        else do
          result <- liftIO $ trackFile env fp (Just $ T.strip name)
          case result of
            Left err -> stError .= Just [show err]
            Right () -> do
              stPopup .= Nothing
              stAssignEditing .= False
              stFocus .= FTracked
              st' <- liftIO $ syncAll st
              put st'
