module Dotf.Tui.Tab.Dotfiles (
  drawDotfilesTab,
) where

import           Brick
import           Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.List   as L
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Dotf.Tui.Theme
import           Dotf.Tui.Types
import           Dotf.Tui.Widgets     (titleWidget)
import           Dotf.Types           (FileStatus (..), RelPath)
import           Lens.Micro           ((^.))

-- | Draw the Dotfiles tab: tracked (left) + untracked (right).
drawDotfilesTab :: State -> Widget RName
drawDotfilesTab st =
  trackedPane <+> untrackedPane
  where
    tFocus = st ^. stFocus == FTracked
    uFocus = st ^. stFocus == FUntracked
    tList  = st ^. stTrackedList
    uList  = st ^. stUntrackedList
    sel    = st ^. stSelected
    frozen = st ^. stFrozen
    tCount = V.length $ L.listElements tList
    uCount = V.length $ L.listElements uList
    filterInd = if st ^. stFilterActive then "[F] " else ""
    trackedPane = borderWithLabel
      (titleWidget (filterInd ++ " Tracked " ++ show tCount ++ " ") tFocus)
      (L.renderList (renderGroupItem sel frozen tFocus) tFocus tList)
    untrackedPane = borderWithLabel
      (titleWidget (" Untracked " ++ show uCount ++ " ") uFocus)
      (L.renderList (renderUntrackedItem uFocus) uFocus uList)

-- | Render a grouped item in the tracked list.
renderGroupItem :: Set.Set RelPath -> Set.Set RelPath -> Bool -> Bool -> GroupItem -> Widget RName
renderGroupItem _ _ cFocus sel (GHeader name collapsed) =
  let icon = if collapsed then "+ " else "- "
      a = if cFocus && sel then attrHeaderSel else attrHeader
  in withAttr a $ str $ icon ++ T.unpack name
renderGroupItem _ _ cFocus sel GUnassignedHeader =
  let a = if cFocus && sel then attrHeaderSel else attrHeader
  in withAttr a $ str "- [unassigned]"
renderGroupItem selected _ cFocus sel (GStaged fp _) =
  let a = if cFocus && sel then attrStagedSelItem else attrStagedItem
      mark = if Set.member fp selected then "\x25cf " else "  "
  in withAttr a $ str $ mark ++ fp
renderGroupItem selected _ cFocus sel (GUnstaged fp status _) =
  let a = if cFocus && sel then attrUnstagedSelItem else attrUnstagedItem
      suffix = case status of Deleted -> " [deleted]"; Exists -> ""
      mark = if Set.member fp selected then "\x25cf " else "  "
  in withAttr a $ str $ mark ++ fp ++ suffix
renderGroupItem selected frozen cFocus sel (GTracked fp _) =
  let isFrozen = Set.member fp frozen
      a | isFrozen && cFocus && sel = attrFrozenSelItem
        | isFrozen                  = attrFrozenItem
        | cFocus && sel             = attrSelItem
        | otherwise                 = attrItem
      mark = if Set.member fp selected then "\x25cf " else "  "
      prefix = if isFrozen then "*" else ""
  in withAttr a $ str $ mark ++ prefix ++ fp
renderGroupItem selected frozen cFocus sel (GUnassignedFile fp) =
  let isFrozen = Set.member fp frozen
      a | isFrozen && cFocus && sel = attrFrozenSelItem
        | isFrozen                  = attrFrozenItem
        | cFocus && sel             = attrSelItem
        | otherwise                 = attrItem
      mark = if Set.member fp selected then "\x25cf " else "  "
      prefix = if isFrozen then "*" else ""
  in withAttr a $ str $ mark ++ prefix ++ fp

-- | Render an untracked item.
renderUntrackedItem :: Bool -> Bool -> UntrackedItem -> Widget RName
renderUntrackedItem cFocus sel (UPluginHeader name) =
  let a = if cFocus && sel then attrHeaderSel else attrHeader
  in withAttr a $ str $ "# " ++ T.unpack name
renderUntrackedItem cFocus sel UWatchlistHeader =
  let a = if cFocus && sel then attrHeaderSel else attrHeader
  in withAttr a $ str "# [watchlist]"
renderUntrackedItem cFocus sel (UPluginFile fp _) =
  let a = if cFocus && sel then attrSelItem else attrItem
  in withAttr a $ str $ "  " ++ fp
renderUntrackedItem cFocus sel (UWatchlistFile fp) =
  let a = if cFocus && sel then attrSelItem else attrItem
  in withAttr a $ str $ "  " ++ fp
