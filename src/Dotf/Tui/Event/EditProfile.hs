module Dotf.Tui.Event.EditProfile (
  handleEditProfileEvent,
) where

import           Brick                  (BrickEvent (..))
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Vector            as Vec
import           Dotf.Profile           (updateProfilePlugins)
import           Dotf.Tui.Types
import           Dotf.Types             (PluginName, displayError)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (use, zoom, (.=))

-- | Handle edit profile plugins popup events.
handleEditProfileEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Esc: close
handleEditProfileEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  stPopup .= Nothing
  stEditProfileName .= Nothing
  stFocus .= FProfileList

-- Space: toggle selected plugin
handleEditProfileEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  pl <- use stNewProfilePlugins
  case L.listSelectedElement pl of
    Nothing -> pure ()
    Just (idx, (n, sel)) -> do
      let updated = L.listModify (const (n, not sel)) pl
      stNewProfilePlugins .= L.listMoveTo idx updated

-- Enter: save changes
handleEditProfileEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  case st ^. stEditProfileName of
    Nothing -> pure ()
    Just name -> do
      let env      = st ^. stEnv
          selected = getSelected (st ^. stNewProfilePlugins)
      result <- liftIO $ updateProfilePlugins env name selected
      case result of
        Left err -> stError .= Just [displayError err]
        Right () -> do
          stPopup .= Nothing
          stEditProfileName .= Nothing
          stFocus .= FProfileList
          st' <- get
          st'' <- liftIO $ syncProfiles st'
          put st''

-- Fallback: vi nav on list
handleEditProfileEvent (VtyEvent ev) =
  zoom stNewProfilePlugins $ L.handleListEventVi L.handleListEvent ev
handleEditProfileEvent _ = pure ()

-- | Get selected plugin names from toggle list.
getSelected :: L.List RName (PluginName, Bool) -> [PluginName]
getSelected l =
  let items = Vec.toList $ L.listElements l
  in [n | (n, True) <- items]
