module Dotf.Tui.Widgets (
  tabBar,
  helpBar,
  statusBar,
  titleWidget,
) where

import           Brick
import           Data.Version   (showVersion)
import           Dotf.Tui.Theme
import           Dotf.Tui.Types
import           Lens.Micro     ((^.))
import           Paths_dotf     (version)

-- | Render the tab bar with app name.
tabBar :: State -> Widget RName
tabBar st =
  padLeft (Pad 1) (hBox $ map renderTab [DotfilesTab, PluginsTab, ProfilesTab])
  <+> (padLeft Max . padRight (Pad 1) $ withAttr attrAppName (str appName))
  where
    current = st ^. stTab
    renderTab t =
      let isFocused = t == current
          a = if isFocused then attrTabFocus else attrTab
      in withAttr a $ padRight (Pad 1) $ str (tabLabel t)
    tabLabel DotfilesTab = "Dotfiles"
    tabLabel PluginsTab  = "Plugins"
    tabLabel ProfilesTab = "Profiles"
    appName = "dotf v" ++ showVersion version

-- | Render context-sensitive help bar.
helpBar :: State -> Widget RName
helpBar st = padLeft (Pad 1) $ hBox $ map renderHelp items
  where
    items = case st ^. stPopup of
      Just SavePopup   -> saveHelp
      Just AssignPopup -> assignHelp
      Just IgnorePopup -> ignoreHelp
      Just FilterPopup -> filterHelp
      Nothing -> case st ^. stTab of
        DotfilesTab -> dotfilesHelp
        PluginsTab  -> pluginsHelp
        ProfilesTab -> profilesHelp
    renderHelp (k, d) =
      withAttr attrHelpKey (str k) <+> withAttr attrHelpDesc (str $ d ++ "  ")

    dotfilesHelp =
      [ ("q", "quit"), ("1-3", "tabs"), ("Tab", "focus")
      , ("e", "edit"), ("d", "diff"), ("s", "save")
      , ("a", "assign"), ("u", "untrack"), ("I", "ignore")
      , ("f", "filter"), ("Space", "collapse")
      ]
    pluginsHelp =
      [ ("q", "quit"), ("1-3", "tabs"), ("Tab", "focus")
      , ("n", "new"), ("e", "edit"), ("D", "delete")
      , ("i", "install"), ("r", "remove")
      ]
    profilesHelp =
      [ ("q", "quit"), ("1-3", "tabs"), ("Tab", "focus")
      , ("n", "new"), ("e", "edit"), ("D", "delete")
      , ("a", "activate"), ("x", "deactivate")
      ]
    saveHelp =
      [ ("Space", "toggle"), ("Tab", "focus")
      , ("Enter", "commit+push"), ("Esc", "cancel")
      ]
    assignHelp =
      [ ("Enter", "assign"), ("+", "new plugin"), ("Esc", "cancel")
      ]
    ignoreHelp =
      [ ("Enter", "add to .gitignore"), ("Esc", "cancel")
      ]
    filterHelp =
      [ ("Enter", "apply"), ("Esc", "cancel")
      ]

-- | Render status bar with sync info + coverage.
statusBar :: State -> Widget RName
statusBar st =
  padLeft (Pad 1) $
    syncWidget <+> str "  " <+> coverageWidget
  where
    ah = st ^. stAhead
    bh = st ^. stBehind
    syncWidget
      | ah == 0 && bh == 0 = withAttr attrStatusOk (str "in sync")
      | otherwise = withAttr attrStatusWarn $
          str $ show ah ++ " ahead, " ++ show bh ++ " behind"
    assigned = st ^. stAssignedCount
    total    = st ^. stTotalCount
    coverageWidget = str $ show assigned ++ "/" ++ show total ++ " assigned"

-- | Render a pane title, highlighted if focused.
titleWidget :: String -> Bool -> Widget RName
titleWidget label True  = withAttr attrTitleFocus $ str label
titleWidget label False = withAttr attrTitle $ str label
