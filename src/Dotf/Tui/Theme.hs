module Dotf.Tui.Theme (
  theme,
  attrKey,
  attrBold,
  attrTitle,
  attrTitleFocus,
  attrAppName,
  attrTab,
  attrTabFocus,
  attrItem,
  attrSelItem,
  attrStagedItem,
  attrStagedSelItem,
  attrUnstagedItem,
  attrUnstagedSelItem,
  attrHeader,
  attrHeaderSel,
  attrInstalledItem,
  attrActiveItem,
  attrHelpKey,
  attrHelpDesc,
  attrStatusOk,
  attrStatusDirty,
  attrStatusBehind,
) where

import           Brick                (AttrName, attrName, fg, on)
import           Brick.Themes         (Theme, newTheme)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L
import           Graphics.Vty         (black, bold, brightBlack, brightCyan,
                                       brightGreen, brightMagenta, brightWhite,
                                       brightYellow, green, red, rgbColor,
                                       white, withStyle, yellow)

-- | Gruvbox-inspired theme.
theme :: Theme
theme = newTheme
  (white `on` bg)
  [ -- List
    (L.listAttr, fg brightWhite)
  , (L.listSelectedAttr, fg brightWhite)
  , (L.listSelectedFocusedAttr, black `on` brightYellow)
  -- Border
  , (B.borderAttr, fg white)
  -- Editor
  , (E.editFocusedAttr, fg brightWhite)
  -- Keys / text
  , (attrKey, withStyle (fg brightMagenta) bold)
  , (attrBold, withStyle (fg white) bold)
  , (attrTitle, withStyle (fg brightWhite) bold)
  , (attrTitleFocus, withStyle (fg yellow) bold)
  -- Tabs
  , (attrTab, fg brightBlack)
  , (attrTabFocus, withStyle (fg brightWhite) bold)
  , (attrAppName, withStyle (fg brightCyan) bold)
  -- Items
  , (attrItem, fg brightWhite)
  , (attrSelItem, black `on` yellow)
  , (attrStagedItem, fg green)
  , (attrStagedSelItem, green `on` yellow)
  , (attrUnstagedItem, fg red)
  , (attrUnstagedSelItem, red `on` yellow)
  -- Headers
  , (attrHeader, withStyle (fg brightCyan) bold)
  , (attrHeaderSel, withStyle (black `on` brightYellow) bold)
  -- Status indicators
  , (attrInstalledItem, fg brightGreen)
  , (attrActiveItem, withStyle (fg brightGreen) bold)
  -- Help bar
  , (attrHelpKey, withStyle (fg brightMagenta) bold)
  , (attrHelpDesc, fg brightWhite)
  -- Status bar
  , (attrStatusOk, fg brightGreen)
  , (attrStatusDirty, withStyle (fg red) bold)
  , (attrStatusBehind, withStyle (fg brightGreen) bold)
  ]
  where
    bg = rgbColor (0 :: Integer) 0 0

attrKey, attrBold, attrTitle, attrTitleFocus :: AttrName
attrKey        = attrName "key"
attrBold       = attrName "bold"
attrTitle      = attrName "title"
attrTitleFocus = attrName "title-focus"

attrAppName, attrTab, attrTabFocus :: AttrName
attrAppName = attrName "app-name"
attrTab     = attrName "tab"
attrTabFocus = attrName "tab-focus"

attrItem, attrSelItem :: AttrName
attrItem    = attrName "item"
attrSelItem = attrName "selected-item"

attrStagedItem, attrStagedSelItem :: AttrName
attrStagedItem    = attrName "staged-item"
attrStagedSelItem = attrName "staged-sel-item"

attrUnstagedItem, attrUnstagedSelItem :: AttrName
attrUnstagedItem    = attrName "unstaged-item"
attrUnstagedSelItem = attrName "unstaged-sel-item"

attrHeader, attrHeaderSel :: AttrName
attrHeader    = attrName "header"
attrHeaderSel = attrName "header-sel"

attrInstalledItem, attrActiveItem :: AttrName
attrInstalledItem = attrName "installed-item"
attrActiveItem    = attrName "active-item"

attrHelpKey, attrHelpDesc :: AttrName
attrHelpKey  = attrName "help-key"
attrHelpDesc = attrName "help-desc"

attrStatusOk, attrStatusDirty, attrStatusBehind :: AttrName
attrStatusOk      = attrName "status-ok"
attrStatusDirty   = attrName "status-dirty"
attrStatusBehind  = attrName "status-behind"
