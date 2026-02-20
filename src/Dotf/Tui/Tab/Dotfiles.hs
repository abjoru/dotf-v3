module Dotf.Tui.Tab.Dotfiles (
  drawDotfilesTab,
) where

import           Brick
import           Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.List   as L
import qualified Data.Vector          as V
import           Dotf.Tui.Theme
import           Dotf.Tui.Types
import           Dotf.Tui.Widgets     (titleWidget)
import           Dotf.Types           (FileStatus (..))
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
    tCount = V.length $ L.listElements tList
    uCount = V.length $ L.listElements uList
    filterInd = if st ^. stFilterActive then "[F] " else ""
    trackedPane = borderWithLabel
      (titleWidget (filterInd ++ " Tracked " ++ show tCount ++ " ") tFocus)
      (L.renderList (renderGroupItem tFocus) tFocus tList)
    untrackedPane = borderWithLabel
      (titleWidget (filterInd ++ " Untracked " ++ show uCount ++ " ") uFocus)
      (L.renderList (renderUntrackedItem uFocus) uFocus uList)

-- | Render a grouped item in the tracked list.
renderGroupItem :: Bool -> Bool -> GroupItem -> Widget RName
renderGroupItem _ sel (GHeader name collapsed) =
  let icon = if collapsed then "+ " else "- "
      a = if sel then attrHeaderSel else attrHeader
  in withAttr a $ str $ icon ++ show name
renderGroupItem _ sel GUnassignedHeader =
  let a = if sel then attrHeaderSel else attrHeader
  in withAttr a $ str "- [unassigned]"
renderGroupItem cFocus sel (GStaged fp _) =
  let a = if cFocus && sel then attrStagedSelItem else attrStagedItem
  in withAttr a $ str $ "  " ++ fp
renderGroupItem cFocus sel (GUnstaged fp status _) =
  let a = if cFocus && sel then attrUnstagedSelItem else attrUnstagedItem
      suffix = case status of Deleted -> " [deleted]"; Exists -> ""
  in withAttr a $ str $ "  " ++ fp ++ suffix
renderGroupItem cFocus sel (GTracked fp _) =
  let a = if cFocus && sel then attrSelItem else attrItem
  in withAttr a $ str $ "  " ++ fp
renderGroupItem cFocus sel (GUnassignedFile fp) =
  let a = if cFocus && sel then attrSelItem else attrItem
  in withAttr a $ str $ "  " ++ fp

-- | Render an untracked item.
renderUntrackedItem :: Bool -> Bool -> UntrackedItem -> Widget RName
renderUntrackedItem _ sel (UPluginHeader name) =
  let a = if sel then attrHeaderSel else attrHeader
  in withAttr a $ str $ show name
renderUntrackedItem _ sel UWatchlistHeader =
  let a = if sel then attrHeaderSel else attrHeader
  in withAttr a $ str "[watchlist]"
renderUntrackedItem cFocus sel (UPluginFile fp _) =
  let a = if cFocus && sel then attrSelItem else attrItem
  in withAttr a $ str $ "  " ++ fp
renderUntrackedItem cFocus sel (UWatchlistFile fp) =
  let a = if cFocus && sel then attrSelItem else attrItem
  in withAttr a $ str $ "  " ++ fp
