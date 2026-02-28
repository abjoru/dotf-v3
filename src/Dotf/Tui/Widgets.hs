module Dotf.Tui.Widgets (
  tabBar,
  helpBar,
  titleWidget,
) where

import           Brick
import qualified Brick.Widgets.Center as C
import           Data.List            (intersperse)
import           Dotf.Tui.Theme
import           Dotf.Tui.Types
import           Lens.Micro           ((^.))

-- | Render the tab bar with coverage + sync info on the right.
tabBar :: State -> Widget RName
tabBar st =
  padLeft (Pad 1) (hBox $ map renderTab [DotfilesTab, PluginsTab, ProfilesTab])
  <+> (padLeft Max . padRight (Pad 1) $ hBox
    [ coverageWidget
    , str "  | "
    , syncWidget
    ])
  where
    current = st ^. stTab
    renderTab t =
      let isFocused = t == current
          a = if isFocused then attrTabFocus else attrTab
      in withAttr a $ padRight (Pad 1) $ str (tabLabel t)
    tabLabel DotfilesTab = "[1] Dotfiles"
    tabLabel PluginsTab  = "[2] Plugins"
    tabLabel ProfilesTab = "[3] Profiles"
    assigned = st ^. stAssignedCount
    total    = st ^. stTotalCount
    coverageWidget = str $ show assigned ++ "/" ++ show total ++ " assigned"
    uc = st ^. stUncommitted
    ah = st ^. stAhead
    bh = st ^. stBehind
    syncWidget
      | uc == 0 && ah == 0 && bh == 0 = withAttr attrStatusOk (str "in sync")
      | otherwise = hBox $ intersperse (str " ") $ concat
          [ [ withAttr attrStatusDirty  $ str $ "~" ++ show uc | uc > 0 ]
          , [ withAttr attrStatusDirty  $ str $ "\x2191" ++ show ah | ah > 0 ]
          , [ withAttr attrStatusBehind $ str $ "\x2193" ++ show bh | bh > 0 ]
          ]

-- | Render context-sensitive help bar (v2 style: two lines, centered, bordered).
helpBar :: State -> Widget RName
helpBar st =
  let (l1, l2) = case st ^. stPopup of
        Just SavePopup   -> (saveHelp1, saveHelp2)
        Just AssignPopup -> (assignHelp1, assignHelp2)
        Just IgnorePopup -> (ignoreHelp1, [])
        Just FilterPopup     -> (filterHelp1, [])
        Just NewPluginPopup  -> (newPluginHelp1, [])
        Just NewProfilePopup -> (newProfileHelp1, [])
        Just PackagePopup    -> (packageHelp1, [])
        Nothing -> case st ^. stTab of
          DotfilesTab -> (dotfilesHelp1, dotfilesHelp2)
          PluginsTab  -> (pluginsHelp1, pluginsHelp2)
          ProfilesTab -> (profilesHelp1, profilesHelp2)
      renderLine items = C.hCenter $ hBox $ map renderHelp items
      renderHelp (k, d) =
        withAttr attrHelpKey (str $ k ++ ":") <+> withAttr attrHelpDesc (str $ " " ++ d ++ " ")
  in case l2 of
    [] -> renderLine l1
    _  -> renderLine l1 <=> renderLine l2

    where
      dotfilesHelp1 =
        [ ("j/k", "Up/Down"), ("Tab", "Switch Focus")
        , ("f/F", "Filter/Clear"), ("Space", "Select")
        , ("Ret", "Collapse"), ("q", "Quit")
        ]
      dotfilesHelp2 =
        [ ("e", "Edit"), ("d", "Diff"), ("s", "Save")
        , ("a", "Assign"), ("u", "Untrack"), ("I", "Ignore")
        , ("z", "Freeze"), ("Z", "Unfreeze")
        ]
      pluginsHelp1 =
        [ ("j/k", "Up/Down"), ("Tab", "Switch Focus"), ("q", "Quit")
        ]
      pluginsHelp2 =
        [ ("n", "New"), ("e", "Edit"), ("D", "Delete")
        , ("i", "Install"), ("r", "Remove"), ("v", "Toggle Details")
        ]
      profilesHelp1 =
        [ ("j/k", "Up/Down"), ("Tab", "Switch Focus"), ("q", "Quit")
        ]
      profilesHelp2 =
        [ ("n", "New"), ("e", "Edit"), ("D", "Delete")
        , ("a", "Activate"), ("x", "Deactivate")
        ]
      saveHelp1 =
        [ ("Space", "Toggle"), ("Tab", "Switch Focus")
        ]
      saveHelp2 =
        [ ("Enter", "Commit+Push"), ("Esc", "Cancel")
        ]
      assignHelp1 =
        [ ("Enter", "Assign"), ("+", "New Plugin"), ("Esc", "Cancel")
        ]
      assignHelp2 = []
      ignoreHelp1 =
        [ ("Enter", "Add to .gitignore"), ("Esc", "Cancel")
        ]
      filterHelp1 =
        [ ("Enter", "Apply"), ("Esc", "Cancel")
        ]
      newPluginHelp1 =
        [ ("Tab", "Switch Field"), ("Enter", "Create"), ("Esc", "Cancel")
        ]
      newProfileHelp1 =
        [ ("Tab", "Switch Field"), ("Space", "Toggle"), ("Enter", "Create"), ("Esc", "Cancel")
        ]
      packageHelp1 =
        [ ("Space", "Toggle"), ("a", "All"), ("n", "None"), ("Enter", "Install"), ("Esc", "Skip")
        ]

-- | Render a pane title, highlighted if focused.
titleWidget :: String -> Bool -> Widget RName
titleWidget label True  = withAttr attrTitleFocus $ str label
titleWidget label False = withAttr attrTitle $ str label
