module Dotf.Tui.Event.Ignore (
  handleIgnoreEvent,
) where

import           Brick                  (BrickEvent (..))
import           Brick.Types            (EventM, get, put)
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           Dotf.Tracking          (ignorePattern)
import           Dotf.Tui.Types
import           Dotf.Types             (displayError)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Lens.Micro.Mtl         (zoom, (.=))

-- | Handle ignore popup events.
handleIgnoreEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Esc: cancel
handleIgnoreEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  stPopup .= Nothing
  stFocus .= FUntracked

-- Enter: add selected segment to .gitignore
handleIgnoreEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let env = st ^. stEnv
  case L.listSelectedElement (st ^. stIgnoreList) of
    Nothing -> stError .= Just ["No pattern selected"]
    Just (_, selected) -> do
      let pat = T.pack selected
      result <- liftIO $ ignorePattern env pat
      case result of
        Left err -> stError .= Just [displayError err]
        Right () -> do
          stPopup .= Nothing
          stFocus .= FUntracked
          updated <- get
          st' <- liftIO $ syncDotfiles updated
          put st'

-- List navigation (vi keys + arrows)
handleIgnoreEvent (VtyEvent ev) =
  zoom stIgnoreList $ L.handleListEventVi L.handleListEvent ev

handleIgnoreEvent _ = pure ()
