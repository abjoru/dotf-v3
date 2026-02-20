module MigrationSpec (spec) where

import           Control.Exception (evaluate)
import           Dotf.Commands     (migrate, scaffoldMetadata)
import           Dotf.Git
import           Dotf.Plugin
import           Dotf.Profile
import           Dotf.State
import           Dotf.Tracking
import           Dotf.Types
import           Dotf.Utils        (pluginsFile, profilesFile)
import           System.Directory  (doesFileExist)
import           System.FilePath   ((</>))
import           Test.Hspec
import           TestUtils

-- | Strict readFile to avoid lazy IO file locking issues.
readFile' :: FilePath -> IO String
readFile' path = do
  s <- readFile path
  evaluate (length s) >> pure s

spec :: Spec
spec = do
  describe "Migration" $ do
    it "scaffolds metadata files" $ withTestEnv $ \env -> do
      result <- migrate env
      result `shouldBe` Right ()
      doesFileExist (pluginsFile env) >>= (`shouldBe` True)
      doesFileExist (profilesFile env) >>= (`shouldBe` True)

    it "is idempotent" $ withTestEnv $ \env -> do
      r1 <- migrate env
      r1 `shouldBe` Right ()
      contents1 <- readFile' (pluginsFile env)
      r2 <- migrate env
      r2 `shouldBe` Right ()
      contents2 <- readFile' (pluginsFile env)
      contents1 `shouldBe` contents2

    it "metadata files are git-added" $ withTestEnv $ \env -> do
      _ <- migrate env
      staged <- gitTrackedStaged env
      case staged of
        Left err -> expectationFailure $ show err
        Right files -> do
          files `shouldContain` [".config/dotf/plugins.yaml"]
          files `shouldContain` [".config/dotf/profiles.yaml"]

    it "full migrate → organize → activate flow" $ withTestEnv $ \env -> do
      let home = _geHome env
      -- Create dotfiles and commit them (simulates pre-existing bare repo)
      writeFile (home </> ".zshrc") "# zshrc"
      writeFile (home </> ".gitconfig") "[user]\n  name = test"
      _ <- gitAdd env ".zshrc"
      _ <- gitAdd env ".gitconfig"
      _ <- gitCommit env "initial dotfiles"

      -- Step 1: migrate — scaffold metadata
      r1 <- migrate env
      r1 `shouldBe` Right ()

      -- Step 2: organize — create plugin, assign files
      r2 <- createPlugin env "shell" (Just "Shell config")
      r2 `shouldBe` Right ()
      r3 <- trackFile env ".zshrc" (Just "shell")
      r3 `shouldBe` Right ()

      r4 <- createPlugin env "git" (Just "Git config")
      r4 `shouldBe` Right ()
      r5 <- trackFile env ".gitconfig" (Just "git")
      r5 `shouldBe` Right ()

      -- Step 3: create profile with both plugins
      r6 <- createProfile env "default" ["shell", "git"]
      r6 `shouldBe` Right ()

      -- Step 4: activate profile
      r7 <- activateProfile env "default"
      r7 `shouldBe` Right ()

      -- Verify state
      st <- loadLocalState env
      _lsActiveProfile st `shouldBe` Just "default"
      _lsInstalledPlugins st `shouldContain` ["shell"]
      _lsInstalledPlugins st `shouldContain` ["git"]

    it "fails on missing bare repo" $ do
      let env = GitEnv "/tmp/dotf-test-nonexistent"
      result <- migrate env
      case result of
        Left _  -> pure ()
        Right _ -> expectationFailure "Expected error for missing repo"

    it "scaffoldMetadata creates directory" $ withTestEnv $ \env -> do
      scaffoldMetadata env
      doesFileExist (pluginsFile env) >>= (`shouldBe` True)
      doesFileExist (profilesFile env) >>= (`shouldBe` True)
