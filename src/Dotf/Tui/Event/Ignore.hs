module Dotf.Tui.Event.Ignore (
  handleIgnoreEvent,
) where

import           Brick                  (BrickEvent (..))
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.Edit     as E
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           Dotf.Tracking          (ignorePattern)
import           Dotf.Tui.Types
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (zoom, (.=))

-- | Handle ignore popup events.
handleIgnoreEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Esc: cancel
handleIgnoreEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  stPopup .= Nothing
  stFocus .= FUntracked

-- Enter: add to .gitignore
handleIgnoreEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let env     = st ^. stEnv
      pattern' = T.pack $ concat $ E.getEditContents (st ^. stIgnoreEditor)
  if T.null (T.strip pattern')
    then stError .= Just ["Pattern cannot be empty"]
    else do
      result <- liftIO $ ignorePattern env (T.strip pattern')
      case result of
        Left err -> stError .= Just [show err]
        Right () -> do
          stPopup .= Nothing
          stFocus .= FUntracked
          st' <- liftIO $ syncDotfiles st
          put st'

-- Editor events
handleIgnoreEvent (VtyEvent ev) =
  zoom stIgnoreEditor $ E.handleEditorEvent (VtyEvent ev)

handleIgnoreEvent _ = pure ()
