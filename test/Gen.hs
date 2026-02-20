module Gen (
  genPluginName,
  genProfileName,
  genRelPath,
  genPlugin,
  genProfile,
  genWatchlist,
  genPluginConfig,
  genProfileConfig,
  genLocalState,
  genHook,
) where

import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Dotf.Types
import           Hedgehog
import qualified Hedgehog.Gen    as Gen
import qualified Hedgehog.Range  as Range

genPluginName :: Gen PluginName
genPluginName = Gen.element
  [ "shell", "git", "neovim", "linux-desktop", "waybar"
  , "kitty", "hyprland", "dunst", "rofi", "tmux"
  ]

genProfileName :: Gen ProfileName
genProfileName = Gen.element
  [ "work-mac", "linux-desktop", "server", "minimal", "full"
  ]

genRelPath :: Gen RelPath
genRelPath = do
  segments <- Gen.list (Range.linear 1 4) genSegment
  pure $ foldr1 (\a b -> a ++ "/" ++ b) segments
  where
    genSegment = do
      dot <- Gen.bool
      name <- Gen.string (Range.linear 1 12) Gen.alphaNum
      pure $ if dot then "." ++ name else name

genHook :: Gen Hook
genHook = Gen.choice
  [ InlineHook <$> Gen.list (Range.linear 1 3) genCmdText
  , ScriptHook <$> genRelPath
  ]
  where
    genCmdText :: Gen Text
    genCmdText = T.pack <$> Gen.string (Range.linear 3 30) Gen.alphaNum

genPlugin :: Gen Plugin
genPlugin = do
  n <- genPluginName
  desc <- Gen.maybe (T.pack <$> Gen.string (Range.linear 5 30) Gen.alphaNum)
  paths <- Gen.list (Range.linear 0 5) genRelPath
  deps <- Gen.list (Range.linear 0 2) genPluginName
  hook <- Gen.maybe genHook
  pure $ Plugin n desc paths deps hook

genProfile :: Gen Profile
genProfile = Profile
  <$> genProfileName
  <*> Gen.list (Range.linear 1 5) genPluginName

genWatchlist :: Gen Watchlist
genWatchlist = Watchlist <$> Gen.list (Range.linear 0 5) genRelPath

genPluginConfig :: Gen PluginConfig
genPluginConfig = do
  plugins <- Gen.list (Range.linear 0 5) genPlugin
  let pluginMap = Map.fromList [(p_pluginName p, p) | p <- plugins]
  wl <- genWatchlist
  pure $ PluginConfig pluginMap wl
  where p_pluginName (Plugin n _ _ _ _) = n

genProfileConfig :: Gen ProfileConfig
genProfileConfig = do
  profiles <- Gen.list (Range.linear 0 4) genProfile
  let profileMap = Map.fromList [(p_profileName p, p) | p <- profiles]
  pure $ ProfileConfig profileMap
  where p_profileName (Profile n _) = n

genLocalState :: Gen LocalState
genLocalState = LocalState
  <$> Gen.maybe genProfileName
  <*> Gen.list (Range.linear 0 5) genPluginName
