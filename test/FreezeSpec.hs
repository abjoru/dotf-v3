module FreezeSpec (spec) where

import           Dotf.Git
import           Dotf.Tracking
import           Test.Hspec
import           TestUtils

spec :: Spec
spec = do
  describe "Freeze" $ do
    it "freeze/unfreeze/listFrozen round-trip" $ withTestEnv $ \env -> do
      setupTestFiles env
      -- Track a file first
      r0 <- gitAdd env ".zshrc"
      r0 `shouldBe` Right ()
      r0c <- gitCommit env "initial"
      r0c `shouldBe` Right ()
      -- Freeze it
      r1 <- freezeFile env ".zshrc"
      r1 `shouldBe` Right ()
      -- List frozen should contain .zshrc
      r2 <- listFrozen env
      case r2 of
        Left err  -> expectationFailure $ show err
        Right fps -> fps `shouldContain` [".zshrc"]
      -- Unfreeze it
      r3 <- unfreezeFile env ".zshrc"
      r3 `shouldBe` Right ()
      -- List frozen should be empty
      r4 <- listFrozen env
      case r4 of
        Left err  -> expectationFailure $ show err
        Right fps -> fps `shouldNotContain` [".zshrc"]

    it "freeze untracked file fails" $ withTestEnv $ \env -> do
      setupTestFiles env
      r <- freezeFile env ".zshrc"
      case r of
        Left _  -> pure ()
        Right _ -> expectationFailure "Expected error freezing untracked file"
