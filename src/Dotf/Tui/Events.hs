module Dotf.Tui.Events (
  handleEvent,
) where

import           Brick                     (BrickEvent (..), halt,
                                            suspendAndResume, vScrollBy,
                                            viewportScroll)
import           Brick.Types               (EventM, get, put)
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.Set                  as Set
import           Dotf.Commands             (runSuggestAssign, runSuggestIgnore)
import           Dotf.Plugin               (deletePlugin, removePlugins)
import           Dotf.Profile              (checkCoverage, deactivateProfile,
                                            deleteProfile)
import           Dotf.Tracking             (freezeFile, unfreezeFile,
                                            untrackFile)
import           Dotf.Tui.Event.Assign     (handleAssignEvent)
import           Dotf.Tui.Event.Dotfiles   (handleDotfilesEvent)
import           Dotf.Tui.Event.Ignore     (handleIgnoreEvent)
import           Dotf.Tui.Event.NewPlugin  (handleNewPluginEvent)
import           Dotf.Tui.Event.NewProfile (handleNewProfileEvent)
import           Dotf.Tui.Event.Packages   (handlePackageEvent)
import           Dotf.Tui.Event.Plugins    (handlePluginsEvent)
import           Dotf.Tui.Event.Profiles   (handleProfilesEvent)
import           Dotf.Tui.Event.Save       (handleSaveEvent)
import           Dotf.Tui.Types
import           Dotf.Types                (_pcPlugins, displayError)
import qualified Graphics.Vty              as V
import           Lens.Micro                ((^.))
import           Lens.Micro.Mtl            (use, zoom, (.=))

-- | Top-level event dispatcher.
-- Priority: error > confirm > popup > global keys > tab handler
handleEvent :: BrickEvent RName DEvent -> EventM RName State ()
handleEvent ev = do
  mErr  <- use stError
  mConf <- use stConfirm
  mPop  <- use stPopup

  case (mErr, mConf, mPop) of
    -- Error dialog: any key dismisses
    (Just _, _, _) -> case ev of
      VtyEvent (V.EvKey _ _) -> stError .= Nothing
      _                      -> pure ()

    -- Confirm dialog: Enter executes, Esc cancels
    (_, Just _, _) -> handleConfirm ev

    -- Popup dispatch
    (_, _, Just SavePopup)       -> handleSaveEvent ev
    (_, _, Just AssignPopup)     -> handleAssignEvent ev
    (_, _, Just IgnorePopup)     -> handleIgnoreEvent ev
    (_, _, Just FilterPopup)     -> handleFilterPopup ev
    (_, _, Just NewPluginPopup)  -> handleNewPluginEvent ev
    (_, _, Just NewProfilePopup) -> handleNewProfileEvent ev
    (_, _, Just PackagePopup)    -> handlePackageEvent ev
    (_, _, Just AiMenuPopup)     -> handleAiMenuEvent ev
    (_, _, Just HelpPopup)       -> handleHelpEvent ev

    -- Global keys (no popup active)
    _ -> handleGlobal ev

-- | Handle global keys, then delegate to tab.
handleGlobal :: BrickEvent RName DEvent -> EventM RName State ()
handleGlobal (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleGlobal (VtyEvent (V.EvKey (V.KChar '1') [])) = switchTab DotfilesTab
handleGlobal (VtyEvent (V.EvKey (V.KChar '2') [])) = switchTab PluginsTab
handleGlobal (VtyEvent (V.EvKey (V.KChar '3') [])) = switchTab ProfilesTab
handleGlobal (VtyEvent (V.EvKey (V.KChar '\t') [])) = cycleFocusForward
handleGlobal (VtyEvent (V.EvKey V.KBackTab [])) = cycleFocusBackward
handleGlobal (VtyEvent (V.EvKey (V.KChar '?') [])) = do
  stPopup .= Just HelpPopup
handleGlobal ev = do
  t <- use stTab
  case t of
    DotfilesTab -> handleDotfilesEvent ev
    PluginsTab  -> handlePluginsEvent ev
    ProfilesTab -> handleProfilesEvent ev

-- | Handle confirm dialog events.
handleConfirm :: BrickEvent RName DEvent -> EventM RName State ()
handleConfirm (VtyEvent (V.EvKey V.KEsc [])) = stConfirm .= Nothing
handleConfirm (VtyEvent (V.EvKey V.KEnter [])) = do
  mConf <- use stConfirm
  stConfirm .= Nothing
  case mConf of
    Just (_, action) -> executeConfirm action
    Nothing          -> pure ()
handleConfirm _ = pure ()

-- | Execute a confirmed action.
executeConfirm :: ConfirmAction -> EventM RName State ()
executeConfirm (ConfirmUntrack fps) = do
  st <- get
  let env = st ^. stEnv
  results <- liftIO $ mapM (untrackFile env) fps
  case [e | Left e <- results] of
    (err:_) -> stError .= Just [displayError err]
    []      -> do
      stSelected .= Set.empty
      st' <- get
      st'' <- liftIO $ syncAll st'
      put st''
executeConfirm (ConfirmDeletePlugin name) = do
  st <- get
  let env = st ^. stEnv
  result <- liftIO $ deletePlugin env name
  case result of
    Left err -> stError .= Just [displayError err]
    Right () -> do
      st' <- liftIO $ syncPlugins st
      put st'
executeConfirm (ConfirmRemovePlugin name) = do
  st <- get
  let env = st ^. stEnv
  result <- liftIO $ removePlugins env [name]
  case result of
    Left err -> stError .= Just [displayError err]
    Right () -> do
      st' <- liftIO $ syncAll st
      put st'
executeConfirm (ConfirmDeleteProfile name) = do
  st <- get
  let env = st ^. stEnv
  result <- liftIO $ deleteProfile env name
  case result of
    Left err -> stError .= Just [displayError err]
    Right () -> do
      st' <- liftIO $ syncProfiles st
      put st'
executeConfirm ConfirmDeactivateProfile = do
  st <- get
  let env = st ^. stEnv
  result <- liftIO $ deactivateProfile env
  case result of
    Left err -> stError .= Just [displayError err]
    Right () -> do
      st' <- liftIO $ syncAll st
      put st'
executeConfirm (ConfirmFreeze fps) = do
  st <- get
  let env = st ^. stEnv
  results <- liftIO $ mapM (freezeFile env) fps
  case [e | Left e <- results] of
    (err:_) -> stError .= Just [displayError err]
    []      -> do
      stSelected .= Set.empty
      st' <- get
      st'' <- liftIO $ syncDotfiles st'
      put st''
executeConfirm (ConfirmUnfreeze fps) = do
  st <- get
  let env = st ^. stEnv
  results <- liftIO $ mapM (unfreezeFile env) fps
  case [e | Left e <- results] of
    (err:_) -> stError .= Just [displayError err]
    []      -> do
      stSelected .= Set.empty
      st' <- get
      st'' <- liftIO $ syncDotfiles st'
      put st''

-- | Handle filter popup events.
handleFilterPopup :: BrickEvent RName DEvent -> EventM RName State ()
handleFilterPopup (VtyEvent (V.EvKey V.KEsc [])) = do
  stFilterActive .= False
  stPopup .= Nothing
  stFocus .= FTracked
  st <- get
  st' <- liftIO $ syncDotfiles st
  put st'
handleFilterPopup (VtyEvent (V.EvKey V.KEnter [])) = do
  stFilterActive .= True
  stPopup .= Nothing
  stFocus .= FTracked
  st <- get
  st' <- liftIO $ syncDotfiles st
  put st'
handleFilterPopup (VtyEvent ev) =
  zoom stFilterEditor $ E.handleEditorEvent (VtyEvent ev)
handleFilterPopup _ = pure ()

-- | Handle AI menu popup events.
handleAiMenuEvent :: BrickEvent RName DEvent -> EventM RName State ()
handleAiMenuEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  stPopup .= Nothing
  stFocus .= FTracked
handleAiMenuEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let ml = L.listSelectedElement (st ^. stAiMenuList)
  case ml of
    Just (_, ("Gitignore", _)) -> do
      let env = st ^. stEnv
      stPopup .= Nothing
      stFocus .= FTracked
      suspendAndResume $ do
        runSuggestIgnore env
        syncDotfiles st
    Just (_, ("Autofill", _)) -> do
      let env     = st ^. stEnv
          pcfg    = st ^. stPluginConfig
          tracked = st ^. stAllTracked
          hasUntracked = not $ null $ L.listElements (st ^. stUntrackedList)
          (_, unassigned) = checkCoverage tracked (_pcPlugins pcfg)
      if null unassigned && not hasUntracked
        then do
          stPopup .= Nothing
          stFocus .= FTracked
          stError .= Just ["No unassigned or untracked files."]
        else do
          stPopup .= Nothing
          stFocus .= FTracked
          suspendAndResume $ do
            runSuggestAssign env
            syncDotfiles st
    _ -> pure ()
handleAiMenuEvent (VtyEvent ev) =
  zoom stAiMenuList $ L.handleListEventVi L.handleListEvent ev
handleAiMenuEvent _ = pure ()

-- | Handle help overlay events.
handleHelpEvent :: BrickEvent RName DEvent -> EventM RName State ()
handleHelpEvent (VtyEvent (V.EvKey V.KEsc []))               = closeHelp
handleHelpEvent (VtyEvent (V.EvKey (V.KChar 'q') []))        = closeHelp
handleHelpEvent (VtyEvent (V.EvKey (V.KChar '?') []))        = closeHelp
handleHelpEvent (VtyEvent (V.EvKey (V.KChar 'j') []))        = helpScroll 1
handleHelpEvent (VtyEvent (V.EvKey V.KDown []))              = helpScroll 1
handleHelpEvent (VtyEvent (V.EvKey (V.KChar 'k') []))        = helpScroll (-1)
handleHelpEvent (VtyEvent (V.EvKey V.KUp []))                = helpScroll (-1)
handleHelpEvent (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = helpScroll 10
handleHelpEvent (VtyEvent (V.EvKey V.KPageDown []))          = helpScroll 10
handleHelpEvent (VtyEvent (V.EvKey (V.KChar 'u') [V.MCtrl])) = helpScroll (-10)
handleHelpEvent (VtyEvent (V.EvKey V.KPageUp []))            = helpScroll (-10)
handleHelpEvent _                                            = pure ()

closeHelp :: EventM RName State ()
closeHelp = stPopup .= Nothing

helpScroll :: Int -> EventM RName State ()
helpScroll n = do
  let vp = viewportScroll RHelpViewport
  vScrollBy vp n

switchTab :: Tab -> EventM RName State ()
switchTab t = do
  stTab .= t
  stFocus .= defaultFocus t

defaultFocus :: Tab -> Focus
defaultFocus DotfilesTab = FTracked
defaultFocus PluginsTab  = FPluginList
defaultFocus ProfilesTab = FProfileList

cycleFocusForward :: EventM RName State ()
cycleFocusForward = do
  f <- use stFocus
  t <- use stTab
  stFocus .= nextFocus t f

cycleFocusBackward :: EventM RName State ()
cycleFocusBackward = do
  f <- use stFocus
  t <- use stTab
  stFocus .= prevFocus t f

nextFocus :: Tab -> Focus -> Focus
nextFocus DotfilesTab FTracked       = FUntracked
nextFocus DotfilesTab FUntracked     = FTracked
nextFocus PluginsTab FPluginList     = FPluginDetail
nextFocus PluginsTab FPluginDetail   = FPluginList
nextFocus ProfilesTab FProfileList   = FProfileDetail
nextFocus ProfilesTab FProfileDetail = FProfileList
nextFocus _ f                        = f

prevFocus :: Tab -> Focus -> Focus
prevFocus DotfilesTab FTracked       = FUntracked
prevFocus DotfilesTab FUntracked     = FTracked
prevFocus PluginsTab FPluginList     = FPluginDetail
prevFocus PluginsTab FPluginDetail   = FPluginList
prevFocus ProfilesTab FProfileList   = FProfileDetail
prevFocus ProfilesTab FProfileDetail = FProfileList
prevFocus _ f                        = f
