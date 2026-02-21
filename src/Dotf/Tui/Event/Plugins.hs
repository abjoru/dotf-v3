module Dotf.Tui.Event.Plugins (
  handlePluginsEvent,
) where

import           Brick                  (BrickEvent (..), suspendAndResume,
                                         vScrollBy, viewportScroll)
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (mapMaybe)
import qualified Data.Text              as T
import           Dotf.Packages
import           Dotf.Plugin            (installPlugins)
import           Dotf.Tui.Types
import           Dotf.Types
import           Dotf.Utils             (editFile, pluginsFile)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (use, zoom, (%=), (.=))

-- | Handle events in Plugins tab.
handlePluginsEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- n: new plugin popup
handlePluginsEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
  st <- get
  put $ openNewPluginPopup st

-- e: edit plugins.yaml
handlePluginsEvent (VtyEvent (V.EvKey (V.KChar 'e') [])) = do
  st <- get
  let env = st ^. stEnv
  suspendAndResume $ do
    editFile (pluginsFile env)
    syncPlugins st

-- D: delete plugin (triggers confirm)
handlePluginsEvent (VtyEvent (V.EvKey (V.KChar 'D') [])) = do
  mPlugin <- getSelectedPlugin
  case mPlugin of
    Nothing -> pure ()
    Just (p, _) ->
      stConfirm .= Just ("Delete plugin " ++ T.unpack (_pluginName p) ++ "?", ConfirmDeletePlugin (_pluginName p))

-- i: install selected plugin
handlePluginsEvent (VtyEvent (V.EvKey (V.KChar 'i') [])) = do
  mPlugin <- getSelectedPlugin
  case mPlugin of
    Nothing -> pure ()
    Just (p, _) -> do
      st <- get
      let env = st ^. stEnv
      result <- liftIO $ installPlugins env [_pluginName p]
      case result of
        Left err -> stError .= Just [displayError err]
        Right () -> do
          -- Check for missing OS packages
          dist <- liftIO detectDistro
          let pcfg = st ^. stPluginConfig
              names = [_pluginName p]
              plugins = mapMaybe (\n -> Map.lookup n (_pcPlugins pcfg)) names
              allPkgs = collectPackages dist plugins
              caskPkgs = collectCaskPackages plugins
          installed <- liftIO $ listInstalledPackages dist
          let missing = filterUninstalled installed allPkgs
          st' <- liftIO $ syncPlugins st
          if null missing
            then put st'
            else put $ openPackagePopup dist missing caskPkgs st'

-- r: remove selected plugin (triggers confirm)
handlePluginsEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
  mPlugin <- getSelectedPlugin
  case mPlugin of
    Nothing -> pure ()
    Just (p, _) ->
      stConfirm .= Just ("Remove plugin " ++ T.unpack (_pluginName p) ++ "?", ConfirmRemovePlugin (_pluginName p))

-- v: toggle simple/advanced detail view
handlePluginsEvent (VtyEvent (V.EvKey (V.KChar 'v') [])) =
  stDetailAdvanced %= not

-- Fallback: delegate to list vi navigation
handlePluginsEvent (VtyEvent ev) = navList ev
handlePluginsEvent _ = pure ()

-- | Navigate plugin list or scroll detail (vi keys enabled).
navList :: V.Event -> EventM RName State ()
navList ev = do
  f <- use stFocus
  case f of
    FPluginList   -> zoom stPluginListW $ L.handleListEventVi L.handleListEvent ev
    FPluginDetail -> case ev of
      V.EvKey (V.KChar 'j') [] -> vScrollBy (viewportScroll RPluginDetail) 1
      V.EvKey V.KDown []       -> vScrollBy (viewportScroll RPluginDetail) 1
      V.EvKey (V.KChar 'k') [] -> vScrollBy (viewportScroll RPluginDetail) (-1)
      V.EvKey V.KUp []         -> vScrollBy (viewportScroll RPluginDetail) (-1)
      _                        -> pure ()
    _             -> pure ()

-- | Get currently selected plugin.
getSelectedPlugin :: EventM RName State (Maybe (Plugin, Bool))
getSelectedPlugin = do
  pl <- use stPluginListW
  pure $ snd <$> L.listSelectedElement pl
