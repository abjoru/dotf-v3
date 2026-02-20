module TestUtils (
  withTestEnv,
  setupTestFiles,
) where

import           Dotf.Git
import           Dotf.Types
import           Dotf.Utils       (metadataDir, pluginsFile, profilesFile)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   removeDirectoryRecursive)
import           System.FilePath  ((</>))
import           Test.Hspec

-- | Create a temp environment with a bare repo for testing.
withTestEnv :: (GitEnv -> IO ()) -> IO ()
withTestEnv action = do
  let tmpDir = "/tmp/dotf-test-integration"
  -- Clean up any previous test
  exists <- doesDirectoryExist tmpDir
  if exists then removeDirectoryRecursive tmpDir else pure ()
  createDirectoryIfMissing True tmpDir
  let env = GitEnv tmpDir
  -- Initialize bare repo
  result <- gitInitBare env
  case result of
    Left err -> expectationFailure $ "Failed to init bare repo: " ++ show err
    Right () -> do
      -- Configure git user for CI environments
      _ <- runGit env ["config", "user.email", "test@test.com"]
      _ <- runGit env ["config", "user.name", "Test"]
      action env
      -- Cleanup
      removeDirectoryRecursive tmpDir

-- | Create test files and scaffold metadata.
setupTestFiles :: GitEnv -> IO ()
setupTestFiles env = do
  let home = _geHome env
  -- Create some dotfiles
  writeFile (home </> ".zshrc") "# zshrc"
  writeFile (home </> ".zprofile") "# zprofile"
  writeFile (home </> ".gitconfig") "[user]\n  name = test"
  createDirectoryIfMissing True (home </> ".config" </> "nvim")
  writeFile (home </> ".config" </> "nvim" </> "init.lua") "-- nvim config"
  -- Scaffold metadata
  createDirectoryIfMissing True (metadataDir env)
  writeFile (pluginsFile env) "plugins: {}\nwatchlist: []"
  writeFile (profilesFile env) "profiles: {}"
