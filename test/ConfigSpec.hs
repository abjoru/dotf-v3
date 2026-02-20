module ConfigSpec (spec) where

import           Data.ByteString     (ByteString)
import qualified Data.Map.Strict     as Map
import qualified Data.Yaml           as Y
import           Dotf.Config
import           Dotf.Types
import           Gen                 (genPluginConfig, genProfileConfig, genLocalState)
import           Hedgehog
import           Test.Hspec
import           Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "YAML round-trip" $ do
    it "PluginConfig" $ hedgehog $ do
      cfg <- forAll genPluginConfig
      let encoded = Y.encode cfg
      case Y.decodeEither' encoded :: Either Y.ParseException PluginConfig of
        Left err  -> do annotateShow err; failure
        Right cfg' -> cfg === cfg'

    it "ProfileConfig" $ hedgehog $ do
      cfg <- forAll genProfileConfig
      let encoded = Y.encode cfg
      case Y.decodeEither' encoded :: Either Y.ParseException ProfileConfig of
        Left err  -> do annotateShow err; failure
        Right cfg' -> cfg === cfg'

    it "LocalState" $ hedgehog $ do
      st <- forAll genLocalState
      let encoded = Y.encode st
      case Y.decodeEither' encoded :: Either Y.ParseException LocalState of
        Left err  -> do annotateShow err; failure
        Right st' -> st === st'

  describe "parsePluginConfig" $ do
    it "parses README example" $ do
      let yaml = Y.decodeEither' examplePluginsYaml :: Either Y.ParseException PluginConfig
      case yaml of
        Left err  -> expectationFailure $ show err
        Right cfg -> do
          Map.size (_pcPlugins cfg) `shouldBe` 4
          length (_wlPaths $ _pcWatchlist cfg) `shouldBe` 3

    it "hook parsing - list becomes InlineHook" $ do
      let yaml = Y.decodeEither' examplePluginsYaml :: Either Y.ParseException PluginConfig
      case yaml of
        Left err  -> expectationFailure $ show err
        Right cfg -> do
          let nvim = _pcPlugins cfg Map.! "neovim"
          _pluginPostInstall nvim `shouldBe` Just (InlineHook ["nvim --headless +PlugInstall +qa"])

    it "hook parsing - string becomes ScriptHook" $ do
      let yaml = Y.decodeEither' examplePluginsYaml :: Either Y.ParseException PluginConfig
      case yaml of
        Left err  -> expectationFailure $ show err
        Right cfg -> do
          let ld = _pcPlugins cfg Map.! "linux-desktop"
          _pluginPostInstall ld `shouldBe` Just (ScriptHook "hooks/linux-desktop/post-install.sh")

  describe "parseProfileConfig" $ do
    it "parses README example" $ do
      let yaml = Y.decodeEither' exampleProfilesYaml :: Either Y.ParseException ProfileConfig
      case yaml of
        Left err  -> expectationFailure $ show err
        Right cfg -> do
          Map.size (_prfProfiles cfg) `shouldBe` 3
          let wm = _prfProfiles cfg Map.! "work-mac"
          _profilePlugins wm `shouldBe` ["shell", "git", "neovim"]

  describe "validatePluginConfig" $ do
    it "passes clean config" $ do
      let cfg = PluginConfig
            (Map.fromList
              [ ("a", Plugin "a" Nothing [".zshrc"] [] Nothing)
              , ("b", Plugin "b" Nothing [".gitconfig"] [] Nothing)
              ])
            (Watchlist [])
      validatePluginConfig cfg `shouldBe` Right ()

    it "rejects duplicate paths" $ do
      let cfg = PluginConfig
            (Map.fromList
              [ ("a", Plugin "a" Nothing [".zshrc"] [] Nothing)
              , ("b", Plugin "b" Nothing [".zshrc"] [] Nothing)
              ])
            (Watchlist [])
      case validatePluginConfig cfg of
        Left (PathConflict _ _ _) -> pure ()
        other -> expectationFailure $ "Expected PathConflict, got: " ++ show other

-- Example YAML from the README
examplePluginsYaml :: ByteString
examplePluginsYaml = Y.encode $ Y.object
  [ "plugins" Y..= Y.object
    [ "shell" Y..= Y.object
      [ "description" Y..= ("Zsh config and shell utilities" :: String)
      , "paths" Y..= ([".zshrc", ".zprofile", ".config/zsh/"] :: [String])
      ]
    , "git" Y..= Y.object
      [ "description" Y..= ("Git configuration" :: String)
      , "paths" Y..= ([".gitconfig"] :: [String])
      ]
    , "neovim" Y..= Y.object
      [ "description" Y..= ("Neovim editor config" :: String)
      , "depends" Y..= ([] :: [String])
      , "paths" Y..= ([".config/nvim/"] :: [String])
      , "post-install" Y..= (["nvim --headless +PlugInstall +qa"] :: [String])
      ]
    , "linux-desktop" Y..= Y.object
      [ "description" Y..= ("Hyprland, Waybar, Kitty, etc." :: String)
      , "depends" Y..= (["shell"] :: [String])
      , "paths" Y..= ([".config/hyprland/", ".config/waybar/", ".config/kitty/", ".config/dunst/", ".config/rofi/"] :: [String])
      , "post-install" Y..= ("hooks/linux-desktop/post-install.sh" :: String)
      ]
    ]
  , "watchlist" Y..= ([".config/", ".local/bin/", ".ssh/"] :: [String])
  ]

exampleProfilesYaml :: ByteString
exampleProfilesYaml = Y.encode $ Y.object
  [ "profiles" Y..= Y.object
    [ "work-mac" Y..= Y.object
      [ "plugins" Y..= (["shell", "git", "neovim"] :: [String])
      ]
    , "linux-desktop" Y..= Y.object
      [ "plugins" Y..= (["shell", "git", "neovim", "linux-desktop"] :: [String])
      ]
    , "server" Y..= Y.object
      [ "plugins" Y..= (["shell", "git"] :: [String])
      ]
    ]
  ]
