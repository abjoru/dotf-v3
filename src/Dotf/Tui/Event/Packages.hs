module Dotf.Tui.Event.Packages (
  handlePackageEvent,
) where

import           Brick              (BrickEvent (..), suspendAndResume)
import           Brick.Types        (EventM, get)
import qualified Brick.Widgets.List as L
import qualified Data.Vector        as Vec
import           Dotf.Packages      (installPackagesCli)
import           Dotf.Tui.Types
import qualified Graphics.Vty       as V
import           Lens.Micro         ((^.))
import           Lens.Micro.Mtl     (use, zoom, (.=))

-- | Handle package popup events.
handlePackageEvent :: BrickEvent RName DEvent -> EventM RName State ()
-- Esc: close without installing
handlePackageEvent (VtyEvent (V.EvKey V.KEsc [])) = closePopup

-- Space: toggle current item
handlePackageEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  pl <- use stPkgItems
  case L.listSelectedElement pl of
    Nothing -> pure ()
    Just (idx, item) -> do
      let toggled = item { _piSelected = not (_piSelected item) }
          items = L.listElements pl
          items' = items Vec.// [(idx, toggled)]
      stPkgItems .= L.listMoveTo idx (L.list RPkgList items' 1)

-- a: select all
handlePackageEvent (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
  pl <- use stPkgItems
  let items' = Vec.map (\i -> i { _piSelected = True }) (L.listElements pl)
      idx = maybe 0 fst (L.listSelectedElement pl)
  stPkgItems .= L.listMoveTo idx (L.list RPkgList items' 1)

-- n: deselect all
handlePackageEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
  pl <- use stPkgItems
  let items' = Vec.map (\i -> i { _piSelected = False }) (L.listElements pl)
      idx = maybe 0 fst (L.listSelectedElement pl)
  stPkgItems .= L.listMoveTo idx (L.list RPkgList items' 1)

-- Enter: install selected packages
handlePackageEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let items    = Vec.toList $ L.listElements (st ^. stPkgItems)
      selected = filter _piSelected items
      dist     = st ^. stPkgDistro
      regular  = map _piName $ filter (not . _piIsCask) selected
      casks    = map _piName $ filter _piIsCask selected
  if null selected
    then closePopup
    else suspendAndResume $ do
      installPackagesCli dist regular casks
      putStrLn "\nPress Enter to continue..."
      _ <- getLine
      pure $ closePopupPure st

-- Fallback: list navigation
handlePackageEvent (VtyEvent ev) =
  zoom stPkgItems $ L.handleListEventVi L.handleListEvent ev
handlePackageEvent _ = pure ()

-- | Close popup, reset focus to tab default.
closePopup :: EventM RName State ()
closePopup = do
  stPopup .= Nothing
  t <- use stTab
  stFocus .= tabDefaultFocus t

-- | Pure version for suspendAndResume.
closePopupPure :: State -> State
closePopupPure st =
  let t = st ^. stTab
  in st { _stPopup = Nothing, _stFocus = tabDefaultFocus t }

tabDefaultFocus :: Tab -> Focus
tabDefaultFocus DotfilesTab = FTracked
tabDefaultFocus PluginsTab  = FPluginList
tabDefaultFocus ProfilesTab = FProfileList
