module Dotf.Tui.Tab.Plugins (
  drawPluginsTab,
) where

import           Brick
import           Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.List   as L
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
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
      (L.renderList renderPluginItem lFocus pList)

    detailPane = borderWithLabel
      (titleWidget " Details " dFocus)
      (renderDetail st)

-- | Render a plugin list item.
renderPluginItem :: Bool -> (Plugin, Bool) -> Widget RName
renderPluginItem sel (p, installed) =
  let name = T.unpack (_pluginName p)
      icon = if installed then "[*] " else "[ ] "
      a | sel && installed = attrInstalledItem
        | sel              = attrSelItem
        | installed        = attrInstalledItem
        | otherwise        = attrItem
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
          files     = maybe [] id $ Map.lookup (_pluginName p) (st ^. stPluginFiles)
          instLabel = if installed then "Yes" else "No"
      in padAll 1 $ vBox
        [ field "Name" name
        , field "Description" desc
        , field "Installed" instLabel
        , field "Dependencies" deps
        , str ""
        , withAttr attrBold $ str "Paths:"
        , vBox $ map (str . ("  " ++)) (if null paths then ["(none)"] else paths)
        , str ""
        , withAttr attrBold $ str "Files:"
        , vBox $ map (str . ("  " ++)) (if null files then ["(none)"] else files)
        ]

-- | Render a labeled field.
field :: String -> String -> Widget n
field label value = withAttr attrBold (str $ label ++ ": ") <+> str value
