module Dotf.Tui.Event.Profiles (
  handleProfilesEvent,
) where

import           Brick                  (BrickEvent (..), suspendAndResume,
                                         vScrollBy, viewportScroll)
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           Dotf.Profile           (activateProfile)
import           Dotf.Tui.Types
import           Dotf.Types
import           Dotf.Utils             (editFile, profilesFile)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (use, zoom, (.=))

-- | Handle events in Profiles tab.
handleProfilesEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- n: new profile popup
handleProfilesEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
  st <- get
  put $ openNewProfilePopup st

-- e: edit profiles.yaml
handleProfilesEvent (VtyEvent (V.EvKey (V.KChar 'e') [])) = do
  st <- get
  let env = st ^. stEnv
  suspendAndResume $ do
    editFile (profilesFile env)
    syncProfiles st

-- D: delete profile (triggers confirm)
handleProfilesEvent (VtyEvent (V.EvKey (V.KChar 'D') [])) = do
  mProf <- getSelectedProfile
  case mProf of
    Nothing -> pure ()
    Just (p, _) ->
      stConfirm .= Just ("Delete profile " ++ T.unpack (_profileName p) ++ "?", ConfirmDeleteProfile (_profileName p))

-- a: activate profile
handleProfilesEvent (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
  mProf <- getSelectedProfile
  case mProf of
    Nothing -> pure ()
    Just (p, _) -> do
      st <- get
      let env = st ^. stEnv
      result <- liftIO $ activateProfile env (_profileName p)
      case result of
        Left err -> stError .= Just [displayError err]
        Right () -> do
          st' <- liftIO $ syncAll st
          put st'

-- x: deactivate profile (triggers confirm)
handleProfilesEvent (VtyEvent (V.EvKey (V.KChar 'x') [])) = do
  mProf <- getSelectedProfile
  case mProf of
    Nothing -> pure ()
    Just (_, active) ->
      if active
      then stConfirm .= Just ("Deactivate current profile?", ConfirmDeactivateProfile)
      else pure ()

-- Fallback: delegate to list vi navigation
handleProfilesEvent (VtyEvent ev) = navList ev
handleProfilesEvent _ = pure ()

-- | Navigate profile list or scroll detail (vi keys enabled).
navList :: V.Event -> EventM RName State ()
navList ev = do
  f <- use stFocus
  case f of
    FProfileList   -> zoom stProfileListW $ L.handleListEventVi L.handleListEvent ev
    FProfileDetail -> case ev of
      V.EvKey (V.KChar 'j') [] -> vScrollBy (viewportScroll RProfileDetail) 1
      V.EvKey V.KDown []       -> vScrollBy (viewportScroll RProfileDetail) 1
      V.EvKey (V.KChar 'k') [] -> vScrollBy (viewportScroll RProfileDetail) (-1)
      V.EvKey V.KUp []         -> vScrollBy (viewportScroll RProfileDetail) (-1)
      _                        -> pure ()
    _              -> pure ()

-- | Get currently selected profile.
getSelectedProfile :: EventM RName State (Maybe (Profile, Bool))
getSelectedProfile = do
  pl <- use stProfileListW
  pure $ snd <$> L.listSelectedElement pl

