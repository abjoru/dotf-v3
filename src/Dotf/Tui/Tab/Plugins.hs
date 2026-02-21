module Dotf.Tui.Tab.Plugins (
  drawPluginsTab,
) where

import           Brick
import           Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.List   as L
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Dotf.Packages        (Distro (..))
import           Dotf.Tui.Theme
import           Dotf.Tui.Types
import           Dotf.Tui.Widgets     (titleWidget)
import           Dotf.Types
import           Lens.Micro           ((^.))

-- | Draw the Plugins tab: list (left) + detail (right).
drawPluginsTab :: State -> Widget RName
drawPluginsTab st =
  hLimitPercent 40 listPane <+> detailPane
  where
    lFocus = st ^. stFocus == FPluginList
    dFocus = st ^. stFocus == FPluginDetail
    pList  = st ^. stPluginListW

    listPane = borderWithLabel
      (titleWidget " Plugins " lFocus)
      (L.renderList (renderPluginItem lFocus) lFocus pList)

    adv = st ^. stDetailAdvanced
    detailPane = borderWithLabel
      (titleWidget (if adv then " Details [+] " else " Details ") dFocus)
      (viewport RPluginDetail Vertical (renderDetail st) <+> fill ' ')

-- | Render a plugin list item.
renderPluginItem :: Bool -> Bool -> (Plugin, Bool) -> Widget RName
renderPluginItem cFocus sel (p, installed) =
  let name = T.unpack (_pluginName p)
      icon = if installed then "[*] " else "[ ] "
      a | cFocus && sel && installed = attrInstalledItem
        | cFocus && sel              = attrSelItem
        | installed                  = attrInstalledItem
        | otherwise                  = attrItem
  in withAttr a $ str $ icon ++ name

-- | Render plugin detail panel.
renderDetail :: State -> Widget RName
renderDetail st =
  case L.listSelectedElement (st ^. stPluginListW) of
    Nothing -> padAll 1 $ str "No plugin selected"
    Just (_, (p, installed)) ->
      let name      = T.unpack (_pluginName p)
          desc      = maybe "(none)" T.unpack (_pluginDescription p)
          deps      = if null (_pluginDepends p) then "(none)"
                      else T.unpack $ T.intercalate ", " (_pluginDepends p)
          paths     = _pluginPaths p
          pkgs      = distroPackages (st ^. stPkgDistro) p
          files     = fromMaybe [] $ Map.lookup (_pluginName p) (st ^. stPluginFiles)
          instLabel = if installed then "Yes" else "No"
          adv       = st ^. stDetailAdvanced
          pathsW    = if adv
            then [ withAttr attrBold $ str $ "Paths (" ++ show (length paths) ++ "):"
                 , vBox $ map (str . ("  " ++)) (if null paths then ["(none)"] else paths)
                 ]
            else [ field "Paths" (show (length paths)) ]
          pkgsW     = case st ^. stPkgDistro of
            UnsupportedDistro -> []
            _ -> if adv
              then [ withAttr attrBold $ str $ "Packages (" ++ show (length pkgs) ++ "):"
                   , vBox $ map (str . ("  " ++) . T.unpack) (if null pkgs then [T.pack "(none)"] else pkgs)
                   ]
              else [ field "Packages" (show (length pkgs)) ]
      in padAll 1 $ vBox $
        [ field "Name" name
        , field "Description" desc
        , field "Installed" instLabel
        , field "Dependencies" deps
        ] ++ pathsW ++ pkgsW ++
        [ str ""
        , withAttr attrBold $ str "Files:"
        , vBox $ map (str . ("  " ++)) (if null files then ["(none)"] else files)
        ]

-- | Get packages for the detected distro.
distroPackages :: Distro -> Plugin -> [Text]
distroPackages Arch p              = _pluginArch p
distroPackages Osx  p              = _pluginOsx p ++ _pluginCask p
distroPackages UnsupportedDistro _ = []

-- | Render a labeled field.
field :: String -> String -> Widget n
field label value = withAttr attrBold (str $ label ++ ": ") <+> str value
