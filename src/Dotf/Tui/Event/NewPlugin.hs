module Dotf.Tui.Event.NewPlugin (
  handleNewPluginEvent,
) where

import           Brick                  (BrickEvent (..))
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.Edit     as E
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           Dotf.Plugin            (createPlugin)
import           Dotf.Tui.Types
import           Dotf.Types             (displayError)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (use, zoom, (.=))

-- | Handle new plugin popup events.
handleNewPluginEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Esc: close
handleNewPluginEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  stPopup .= Nothing
  stFocus .= FPluginList

-- Tab: cycle focus
handleNewPluginEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  f <- use stFocus
  case f of
    FNewPluginName -> stFocus .= FNewPluginDesc
    _              -> stFocus .= FNewPluginName

handleNewPluginEvent (VtyEvent (V.EvKey V.KBackTab [])) = do
  f <- use stFocus
  case f of
    FNewPluginDesc -> stFocus .= FNewPluginName
    _              -> stFocus .= FNewPluginDesc

-- Enter: create plugin
handleNewPluginEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let env  = st ^. stEnv
      name = T.strip $ T.pack $ concat $ E.getEditContents (st ^. stNewPluginName)
      desc = T.strip $ T.pack $ concat $ E.getEditContents (st ^. stNewPluginDesc)
      mDesc = if T.null desc then Nothing else Just desc
  if T.null name
    then stError .= Just ["Plugin name cannot be empty"]
    else do
      result <- liftIO $ createPlugin env name mDesc
      case result of
        Left err -> stError .= Just [displayError err]
        Right () -> do
          stPopup .= Nothing
          stFocus .= FPluginList
          st' <- get
          st'' <- liftIO $ syncPlugins st'
          put st''

-- Fallback: route to focused editor
handleNewPluginEvent (VtyEvent ev) = do
  f <- use stFocus
  case f of
    FNewPluginName -> zoom stNewPluginName $ E.handleEditorEvent (VtyEvent ev)
    FNewPluginDesc -> zoom stNewPluginDesc $ E.handleEditorEvent (VtyEvent ev)
    _              -> pure ()
handleNewPluginEvent _ = pure ()
