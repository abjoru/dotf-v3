module Dotf.Tui.Event.NewProfile (
  handleNewProfileEvent,
) where

import           Brick                  (BrickEvent (..))
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.Edit     as E
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import qualified Data.Vector            as Vec
import           Dotf.Profile           (createProfile)
import           Dotf.Tui.Types
import           Dotf.Types             (PluginName, displayError)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (use, zoom, (.=))

-- | Handle new profile popup events.
handleNewProfileEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Esc: close
handleNewProfileEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  stPopup .= Nothing
  stFocus .= FProfileList

-- Tab: cycle focus
handleNewProfileEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  f <- use stFocus
  case f of
    FNewProfileName -> stFocus .= FNewProfilePlugins
    _               -> stFocus .= FNewProfileName

handleNewProfileEvent (VtyEvent (V.EvKey V.KBackTab [])) = do
  f <- use stFocus
  case f of
    FNewProfilePlugins -> stFocus .= FNewProfileName
    _                  -> stFocus .= FNewProfilePlugins

-- Space: toggle selected plugin
handleNewProfileEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  f <- use stFocus
  case f of
    FNewProfilePlugins -> do
      pl <- use stNewProfilePlugins
      case L.listSelectedElement pl of
        Nothing -> pure ()
        Just (idx, (n, sel)) -> do
          let updated = L.listModify (const (n, not sel)) pl
          stNewProfilePlugins .= L.listMoveTo idx updated
    _ -> pure ()

-- Enter: create profile
handleNewProfileEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let env  = st ^. stEnv
      name = T.strip $ T.pack $ concat $ E.getEditContents (st ^. stNewProfileName)
      selected = getSelected (st ^. stNewProfilePlugins)
  if T.null name
    then stError .= Just ["Profile name cannot be empty"]
    else do
      result <- liftIO $ createProfile env name selected
      case result of
        Left err -> stError .= Just [displayError err]
        Right () -> do
          stPopup .= Nothing
          stFocus .= FProfileList
          st' <- get
          st'' <- liftIO $ syncProfiles st'
          put st''

-- Fallback: route to focused widget
handleNewProfileEvent (VtyEvent ev) = viNavOrEdit ev
handleNewProfileEvent _ = pure ()

-- | Route event to editor or list based on focus.
viNavOrEdit :: V.Event -> EventM RName State ()
viNavOrEdit ev = do
  f <- use stFocus
  case f of
    FNewProfileName    -> zoom stNewProfileName $ E.handleEditorEvent (VtyEvent ev)
    FNewProfilePlugins -> zoom stNewProfilePlugins $ L.handleListEventVi L.handleListEvent ev
    _                  -> pure ()

-- | Get selected plugin names from toggle list.
getSelected :: L.List RName (PluginName, Bool) -> [PluginName]
getSelected l =
  let items = Vec.toList $ L.listElements l
  in [n | (n, True) <- items]
