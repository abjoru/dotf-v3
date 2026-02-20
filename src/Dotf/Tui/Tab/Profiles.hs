module Dotf.Tui.Tab.Profiles (
  drawProfilesTab,
) where

import           Brick
import           Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.List   as L
import qualified Data.Text            as T
import           Dotf.Tui.Theme
import           Dotf.Tui.Types
import           Dotf.Tui.Widgets     (titleWidget)
import           Dotf.Types
import           Lens.Micro           ((^.))

-- | Draw the Profiles tab: list (left) + detail (right).
drawProfilesTab :: State -> Widget RName
drawProfilesTab st =
  hLimitPercent 40 listPane <+> detailPane
  where
    lFocus = st ^. stFocus == FProfileList
    dFocus = st ^. stFocus == FProfileDetail
    pList  = st ^. stProfileListW

    listPane = borderWithLabel
      (titleWidget " Profiles " lFocus)
      (L.renderList renderProfileItem lFocus pList)

    detailPane = borderWithLabel
      (titleWidget " Details " dFocus)
      (viewport RProfileDetail Vertical (renderDetail st) <+> fill ' ')

-- | Render a profile list item.
renderProfileItem :: Bool -> (Profile, Bool) -> Widget RName
renderProfileItem sel (p, active) =
  let name = T.unpack (_profileName p)
      icon = if active then "[*] " else "[ ] "
      a | sel && active = attrActiveItem
        | sel           = attrSelItem
        | active        = attrActiveItem
        | otherwise     = attrItem
  in withAttr a $ str $ icon ++ name

-- | Render profile detail panel.
renderDetail :: State -> Widget RName
renderDetail st =
  case L.listSelectedElement (st ^. stProfileListW) of
    Nothing -> padAll 1 $ str "No profile selected"
    Just (_, (p, active)) ->
      let name       = T.unpack (_profileName p)
          plugins    = _profilePlugins p
          installed  = _lsInstalledPlugins (st ^. stLocalState)
          actLabel   = if active then "Yes" else "No"
      in padAll 1 $ vBox
        [ field "Name" name
        , field "Active" actLabel
        , str ""
        , withAttr attrBold $ str "Plugins:"
        , vBox $ map (renderPluginStatus installed) plugins
        ]

-- | Render a plugin in profile detail with install status.
renderPluginStatus :: [PluginName] -> PluginName -> Widget n
renderPluginStatus installed name =
  let isInstalled = name `elem` installed
      icon = if isInstalled then "[*] " else "[ ] "
      a = if isInstalled then attrInstalledItem else attrItem
  in withAttr a $ str $ "  " ++ icon ++ T.unpack name

-- | Render a labeled field.
field :: String -> String -> Widget n
field label value = withAttr attrBold (str $ label ++ ": ") <+> str value
