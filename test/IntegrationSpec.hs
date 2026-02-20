module IntegrationSpec (spec) where

import qualified Data.Map.Strict as Map
import           Dotf.Config
import           Dotf.Git
import           Dotf.Plugin
import           Dotf.Profile
import           Dotf.State
import           Dotf.Tracking
import           Dotf.Types
import           Test.Hspec
import           TestUtils

spec :: Spec
spec = do
  describe "Integration" $ do
    it "plugin CRUD" $ withTestEnv $ \env -> do
      setupTestFiles env
      -- Create plugins
      r1 <- createPlugin env "shell" (Just "Shell config")
      r1 `shouldBe` Right ()
      r2 <- createPlugin env "git" (Just "Git config")
      r2 `shouldBe` Right ()
      -- Verify list
      cfg <- loadPluginConfig env
      case cfg of
        Left err -> expectationFailure $ show err
        Right c  -> Map.size (_pcPlugins c) `shouldBe` 2
      -- Delete plugin
      r3 <- deletePlugin env "git"
      r3 `shouldBe` Right ()
      cfg2 <- loadPluginConfig env
      case cfg2 of
        Left err -> expectationFailure $ show err
        Right c  -> Map.size (_pcPlugins c) `shouldBe` 1

    it "track/untrack round-trip" $ withTestEnv $ \env -> do
      setupTestFiles env
      -- Create plugin first
      _ <- createPlugin env "shell" Nothing
      -- Track a file
      r1 <- trackFile env ".zshrc" (Just "shell")
      r1 `shouldBe` Right ()
      -- Verify plugin has the path
      cfg <- loadPluginConfig env
      case cfg of
        Left err -> expectationFailure $ show err
        Right c  -> do
          let shell = _pcPlugins c Map.! "shell"
          _pluginPaths shell `shouldContain` [".zshrc"]
      -- Untrack
      r2 <- untrackFile env ".zshrc"
      r2 `shouldBe` Right ()
      -- Verify removed from plugin
      cfg2 <- loadPluginConfig env
      case cfg2 of
        Left err -> expectationFailure $ show err
        Right c  -> do
          let shell = _pcPlugins c Map.! "shell"
          _pluginPaths shell `shouldNotContain` [".zshrc"]

    it "dependency resolution auto-installs deps" $ withTestEnv $ \env -> do
      setupTestFiles env
      -- Create plugins with dependencies
      _ <- createPlugin env "base" (Just "Base")
      _ <- createPlugin env "shell" (Just "Shell")
      -- Manually set dependency
      cfg <- loadPluginConfig env
      case cfg of
        Left err -> expectationFailure $ show err
        Right c  -> do
          let shell = (_pcPlugins c Map.! "shell") { _pluginDepends = ["base"] }
              newCfg = c { _pcPlugins = Map.insert "shell" shell (_pcPlugins c) }
          savePluginConfig env newCfg
      -- Install shell (should auto-install base)
      r <- installPlugins env ["shell"]
      r `shouldBe` Right ()
      -- Verify both installed
      st <- loadLocalState env
      _lsInstalledPlugins st `shouldContain` ["base"]
      _lsInstalledPlugins st `shouldContain` ["shell"]

    it "profile create/activate blocks on unassigned files" $ withTestEnv $ \env -> do
      setupTestFiles env
      -- Track files without assigning to plugins
      r0a <- gitAdd env ".zshrc"
      r0a `shouldBe` Right ()
      r0b <- gitCommit env "initial"
      r0b `shouldBe` Right ()
      -- Create profile
      _ <- createPlugin env "shell" Nothing
      r1 <- createProfile env "test" ["shell"]
      r1 `shouldBe` Right ()
      -- Activate should fail (unassigned files)
      r2 <- activateProfile env "test"
      case r2 of
        Left (UnassignedFilesExist _) -> pure ()
        other -> expectationFailure $ "Expected UnassignedFilesExist, got: " ++ show other

    it "state persistence" $ withTestEnv $ \env -> do
      setupTestFiles env
      -- Save state
      let st = LocalState (Just "linux") ["shell", "git"]
      saveLocalState env st
      -- Load it back
      st' <- loadLocalState env
      st' `shouldBe` st

    it "watchlist CRUD" $ withTestEnv $ \env -> do
      setupTestFiles env
      -- Add to watchlist
      r1 <- addWatchPath env ".config/"
      r1 `shouldBe` Right ()
      r2 <- addWatchPath env ".local/bin/"
      r2 `shouldBe` Right ()
      -- List
      r3 <- listWatchPaths env
      case r3 of
        Left err    -> expectationFailure $ show err
        Right paths -> length paths `shouldBe` 2
      -- Remove
      r4 <- removeWatchPath env ".config/"
      r4 `shouldBe` Right ()
      r5 <- listWatchPaths env
      case r5 of
        Left err    -> expectationFailure $ show err
        Right paths -> length paths `shouldBe` 1

    it "classifyUntracked sorts correctly" $ do
      let plugins = Map.fromList
            [ ("nvim", Plugin "nvim" Nothing [".config/nvim/"] [] Nothing)
            , ("shell", Plugin "shell" Nothing [".zshrc"] [] Nothing)
            ]
          watchPaths = [".config/"]
          files = [".config/nvim/newfile.lua", ".config/kitty/kitty.conf", ".random"]
          (plugScoped, wl) = classifyUntracked files plugins watchPaths
      -- nvim file goes to nvim bucket
      Map.lookup "nvim" plugScoped `shouldBe` Just [".config/nvim/newfile.lua"]
      -- kitty file goes to watchlist (under .config/ but no matching plugin)
      wl `shouldBe` [".config/kitty/kitty.conf"]
      -- .random goes nowhere (not in plugin or watchlist)

    it "error cases - path conflict" $ do
      let cfg = PluginConfig
            (Map.fromList
              [ ("a", Plugin "a" Nothing [".zshrc"] [] Nothing)
              , ("b", Plugin "b" Nothing [".zshrc", ".gitconfig"] [] Nothing)
              ])
            (Watchlist [])
      case validatePluginConfig cfg of
        Left (PathConflict _ _ ".zshrc") -> pure ()
        other -> expectationFailure $ "Expected PathConflict for .zshrc, got: " ++ show other

    it "error cases - removing depended plugin" $ do
      let plugins = Map.fromList
            [ ("base", Plugin "base" Nothing [] [] Nothing)
            , ("shell", Plugin "shell" Nothing [] ["base"] Nothing)
            ]
      case checkRemoveSafety plugins ["base", "shell"] ["base"] of
        Left (DependencyError _) -> pure ()
        other -> expectationFailure $ "Expected DependencyError, got: " ++ show other
