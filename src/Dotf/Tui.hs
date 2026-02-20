module Dotf.Tui (
  tui,
) where

import           Brick
import           Brick.Themes          (themeToAttrMap)
import qualified Brick.Widgets.Border  as B
import qualified Brick.Widgets.Center  as C
import qualified Brick.Widgets.Edit    as E
import qualified Brick.Widgets.List    as L
import           Dotf.Tui.Events       (handleEvent)
import           Dotf.Tui.Tab.Dotfiles (drawDotfilesTab)
import           Dotf.Tui.Tab.Plugins  (drawPluginsTab)
import           Dotf.Tui.Tab.Profiles (drawProfilesTab)
import           Dotf.Tui.Theme
import           Dotf.Tui.Types
import           Dotf.Tui.Widgets      (helpBar, tabBar)
import           Dotf.Types            (GitEnv, PluginName)
import           Lens.Micro            ((^.))

-- | Launch the TUI.
tui :: GitEnv -> IO ()
tui env = do
  initialState <- buildState env
  _ <- defaultMain app initialState
  pure ()

-- | Brick app definition.
app :: App State DEvent RName
app = App
  { appDraw         = drawUI
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const $ themeToAttrMap theme
  }

-- | Cursor selection for editor widgets.
chooseCursor :: State -> [CursorLocation RName] -> Maybe (CursorLocation RName)
chooseCursor st = case st ^. stPopup of
  Just SavePopup   | st ^. stFocus == FSaveEditor   -> showCursorNamed RCommitEditor
  Just AssignPopup | st ^. stFocus == FAssignEditor -> showCursorNamed RAssignEditor
  Just IgnorePopup  -> const Nothing
  Just FilterPopup  -> showCursorNamed RFilterEditor
  _                 -> neverShowCursor st

-- | Main draw function. Returns popup layer over main layer.
drawUI :: State -> [Widget RName]
drawUI st = popupLayer ++ [mainLayer]
  where
    mainLayer = vBox
      [ tabBar st
      , B.hBorder
      , tabContent st
      , B.border $ helpBar st
      ]
    popupLayer = case st ^. stError of
      Just msgs -> [errorDialog msgs]
      Nothing -> case st ^. stConfirm of
        Just (msg, _) -> [confirmDialog msg]
        Nothing -> case st ^. stPopup of
          Just SavePopup   -> [drawSavePopup st]
          Just AssignPopup -> [drawAssignPopup st]
          Just IgnorePopup -> [drawIgnorePopup st]
          Just FilterPopup -> [drawFilterPopup st]
          Nothing          -> []

-- | Render tab body.
tabContent :: State -> Widget RName
tabContent st = case st ^. stTab of
  DotfilesTab -> drawDotfilesTab st
  PluginsTab  -> drawPluginsTab st
  ProfilesTab -> drawProfilesTab st

------------------
-- Popup draws  --
------------------

-- | Error dialog.
errorDialog :: [String] -> Widget RName
errorDialog msgs = C.centerLayer $ B.borderWithLabel (withAttr attrTitleFocus $ str " Error ") $
  padAll 1 $ vBox $ map str (concatMap lines msgs)

-- | Confirm dialog.
confirmDialog :: String -> Widget RName
confirmDialog msg = C.centerLayer $ B.borderWithLabel (withAttr attrTitleFocus $ str " Confirm ") $
  padAll 1 $ vBox
    [ str msg
    , str ""
    , withAttr attrHelpKey (str "Enter") <+> str " confirm  " <+>
      withAttr attrHelpKey (str "Esc") <+> str " cancel"
    ]

-- | Save popup.
drawSavePopup :: State -> Widget RName
drawSavePopup st = C.centerLayer $ B.borderWithLabel (withAttr attrTitleFocus $ str " Save ") $
  hLimit 60 $ vLimit 20 $ padAll 1 $ vBox
    [ withAttr attrBold $ str "Files:"
    , vLimit 10 $ L.renderList renderSaveItem (st ^. stFocus == FSaveList) (st ^. stSaveItems)
    , B.hBorder
    , withAttr attrBold $ str "Commit message:"
    , vLimit 3 $ E.renderEditor (str . unlines) (st ^. stFocus == FSaveEditor) (st ^. stCommitEditor)
    ]

-- | Render a save item.
renderSaveItem :: Bool -> SaveItem -> Widget RName
renderSaveItem sel item =
  let icon = if _siSelected item then "[x] " else "[ ] "
      a | sel && _siIsStaged item = attrStagedSelItem
        | sel                     = attrSelItem
        | _siIsStaged item        = attrStagedItem
        | otherwise               = attrItem
  in withAttr a $ str $ icon ++ _siPath item

-- | Assign popup.
drawAssignPopup :: State -> Widget RName
drawAssignPopup st = C.centerLayer $ B.borderWithLabel (withAttr attrTitleFocus $ str " Assign ") $
  hLimit 50 $ vLimit 15 $ padAll 1 $ vBox $
    [ withAttr attrBold $ str $ case st ^. stAssignFiles of
        []  -> "File: ?"
        [f] -> "File: " ++ f
        fs  -> show (length fs) ++ " files selected"
    , str ""
    , withAttr attrBold $ str "Select plugin:"
    , vLimit 8 $ L.renderList renderAssignItem (st ^. stFocus == FAssignList) (st ^. stAssignList)
    ] ++
    if st ^. stAssignEditing
    then [ str ""
         , withAttr attrBold $ str "New plugin name:"
         , vLimit 1 $ E.renderEditor (str . unlines) True (st ^. stAssignEditor)
         ]
    else [ str ""
         , withAttr attrHelpDesc $ str "Press + to create new plugin"
         ]

-- | Render an assign list item.
renderAssignItem :: Bool -> (PluginName, Bool) -> Widget RName
renderAssignItem sel (name, _) =
  let a = if sel then attrSelItem else attrItem
  in withAttr a $ str $ "  " ++ show name

-- | Ignore popup: path-level selector.
drawIgnorePopup :: State -> Widget RName
drawIgnorePopup st = C.centerLayer $ B.borderWithLabel (withAttr attrTitleFocus $ str " Ignore ") $
  hLimit 60 $ vLimit 12 $ padAll 1 $ vBox
    [ withAttr attrBold $ str "Select path level to ignore:"
    , L.renderList renderIgnoreItem True (st ^. stIgnoreList)
    ]

-- | Render an ignore list item.
renderIgnoreItem :: Bool -> FilePath -> Widget RName
renderIgnoreItem sel fp =
  let a = if sel then attrSelItem else attrItem
  in withAttr a $ str $ "  " ++ fp

-- | Filter popup.
drawFilterPopup :: State -> Widget RName
drawFilterPopup st = C.centerLayer $ B.borderWithLabel (withAttr attrTitleFocus $ str " Filter ") $
  hLimit 50 $ padAll 1 $ vBox
    [ withAttr attrBold $ str "Filter pattern:"
    , vLimit 1 $ E.renderEditor (str . unlines) True (st ^. stFilterEditor)
    ]
