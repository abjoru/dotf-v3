module ProfileSpec (spec) where

import qualified Data.Map.Strict     as Map
import           Dotf.Path           (isSubpathOf)
import           Dotf.Plugin         (managedPaths)
import           Dotf.Profile
import           Dotf.Types
import           Gen                 (genRelPath)
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "checkCoverage" $ do
    it "partition property: assigned + unassigned = input" $ hedgehog $ do
      files <- forAll $ Gen.list (Range.linear 0 20) genRelPath
      let plugins = Map.fromList
            [ ("shell", Plugin "shell" Nothing [".zshrc", ".zprofile"] [] Nothing [] [] [])
            , ("nvim",  Plugin "nvim"  Nothing [".config/nvim/"]       [] Nothing [] [] [])
            ]
          isManagedPath f = any (\m -> m `isSubpathOf` f) managedPaths
          userFiles = filter (not . isManagedPath) files
          (assigned, unassigned) = checkCoverage files plugins
      length assigned + length unassigned === length userFiles

    it "empty plugin map -> all unassigned" $ do
      let files = [".zshrc", ".gitconfig", ".config/nvim/init.lua"]
          (assigned, unassigned) = checkCoverage files Map.empty
      assigned `shouldBe` []
      unassigned `shouldBe` files

    it "excludes .config/dotf/ metadata from coverage" $ do
      let files = [".zshrc", ".config/dotf/plugins.yaml", ".config/dotf/profiles.yaml"]
          (assigned, unassigned) = checkCoverage files Map.empty
      assigned `shouldBe` []
      unassigned `shouldBe` [".zshrc"]

  describe "listProfiles" $ do
    it "marks active profile correctly" $ do
      let profiles = ProfileConfig $ Map.fromList
            [ ("work", Profile "work" ["shell"])
            , ("home", Profile "home" ["shell", "nvim"])
            ]
          state = LocalState (Just "home") ["shell", "nvim"]
          result = listProfiles profiles state
          isActive (p, active) = (_profileName p == "home") == active
      all isActive result `shouldBe` True
