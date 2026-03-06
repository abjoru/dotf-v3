module Dotf.Tui.Tab.Profiles (
  drawProfilesTab,
) where

import           Brick
import           Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.List   as L
import qualified Data.Set             as Set
import qualified Data.Text            as T
import           Dotf.Plugin          (resolveDependencies)
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
      (L.renderList (renderProfileItem lFocus) lFocus pList)

    detailPane = borderWithLabel
      (titleWidget " Details " dFocus)
      (viewport RProfileDetail Vertical (renderDetail st) <+> fill ' ')

-- | Render a profile list item.
renderProfileItem :: Bool -> Bool -> (Profile, Bool) -> Widget RName
renderProfileItem cFocus sel (p, active) =
  let name = T.unpack (_profileName p)
      icon = if active then "[*] " else "[ ] "
      a | cFocus && sel && active = attrActiveItem
        | cFocus && sel           = attrSelItem
        | active                  = attrActiveItem
        | otherwise               = attrItem
  in withAttr a $ str $ icon ++ name

-- | Render profile detail panel.
renderDetail :: State -> Widget RName
renderDetail st =
  case L.listSelectedElement (st ^. stProfileListW) of
    Nothing -> padAll 1 $ str "No profile selected"
    Just (_, (p, active)) ->
      let name       = T.unpack (_profileName p)
          plugins    = _profilePlugins p
          pluginMap  = _pcPlugins (st ^. stPluginConfig)
          installed  = _lsInstalledPlugins (st ^. stLocalState)
          actLabel   = if active then "Yes" else "No"
          directSet  = Set.fromList plugins
          resolved   = case resolveDependencies pluginMap plugins of
                         Right rs -> rs
                         Left _   -> plugins
      in padAll 1 $ vBox
        [ field "Name" name
        , field "Active" actLabel
        , str ""
        , withAttr attrBold $ str "Plugins:"
        , vBox $ map (renderPluginStatus installed directSet) resolved
        ]

-- | Render a plugin in profile detail with install status.
renderPluginStatus :: [PluginName] -> Set.Set PluginName -> PluginName -> Widget n
renderPluginStatus installed directSet name =
  let isInstalled = name `elem` installed
      isDep       = not (Set.member name directSet)
      icon | isInstalled && isDep = "[~] "
           | isInstalled          = "[*] "
           | isDep                = "[~] "
           | otherwise            = "[ ] "
      suffix = if isDep then " (dep)" else ""
      a | isDep       = attrDepItem
        | isInstalled = attrInstalledItem
        | otherwise   = attrItem
  in withAttr a $ str $ "  " ++ icon ++ T.unpack name ++ suffix

-- | Render a labeled field.
field :: String -> String -> Widget n
field label value = withAttr attrBold (str $ label ++ ": ") <+> str value
