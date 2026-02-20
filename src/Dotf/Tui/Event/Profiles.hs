module Dotf.Tui.Event.Profiles (
  handleProfilesEvent,
) where

import           Brick                  (BrickEvent (..), suspendAndResume)
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           Dotf.Profile           (activateProfile, createProfile)
import           Dotf.Tui.Types
import           Dotf.Types
import           Dotf.Utils             (editFile, profilesFile)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (use, zoom, (.=))

-- | Handle events in Profiles tab.
handleProfilesEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Navigation (vi-enabled)
handleProfilesEvent (VtyEvent ev@(V.EvKey (V.KChar 'j') [])) = navList ev
handleProfilesEvent (VtyEvent ev@(V.EvKey (V.KChar 'k') [])) = navList ev
handleProfilesEvent (VtyEvent ev@(V.EvKey (V.KChar 'g') [])) = navList ev
handleProfilesEvent (VtyEvent ev@(V.EvKey (V.KChar 'G') [])) = navList ev
handleProfilesEvent (VtyEvent ev@(V.EvKey V.KDown []))        = navList ev
handleProfilesEvent (VtyEvent ev@(V.EvKey V.KUp []))          = navList ev

-- n: new profile
handleProfilesEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
  st <- get
  let env = st ^. stEnv
  suspendAndResume $ do
    putStr "Profile name: "
    name <- getLine
    putStr "Plugins (comma-separated): "
    pluginsStr <- getLine
    let plugins = map (T.strip . T.pack) $ splitOn ',' pluginsStr
    result <- createProfile env (T.pack name) plugins
    case result of
      Left err -> putStrLn $ "Error: " ++ show err
      Right () -> putStrLn $ "Created profile: " ++ name
    putStrLn "Press Enter to continue..."
    _ <- getLine
    syncProfiles st

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
        Left err -> stError .= Just [show err]
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

handleProfilesEvent _ = pure ()

-- | Navigate profile list (vi keys enabled).
navList :: V.Event -> EventM RName State ()
navList ev = do
  f <- use stFocus
  case f of
    FProfileList -> zoom stProfileListW $ L.handleListEventVi L.handleListEvent ev
    _            -> pure ()

-- | Get currently selected profile.
getSelectedProfile :: EventM RName State (Maybe (Profile, Bool))
getSelectedProfile = do
  pl <- use stProfileListW
  pure $ snd <$> L.listSelectedElement pl

-- | Split a string on a character.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
  | x == c    = "" : splitOn c xs
  | otherwise = case splitOn c xs of
      []    -> [[x]]
      (h:t) -> (x:h) : t
