module PackagesSpec (spec) where

import           Data.List           (nub)
import           Dotf.Packages
import           Dotf.Types
import           Gen                 (genPkgName, genPlugin)
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "collectPackages" $ do
    it "Arch extracts only arch field" $ hedgehog $ do
      plugins <- forAll $ Gen.list (Range.linear 0 5) genPlugin
      let result = collectPackages Arch plugins
          allArch = concatMap _pluginArch plugins
      -- Every collected package must come from arch fields
      assert $ all (`elem` allArch) result

    it "Osx extracts osx + cask fields" $ hedgehog $ do
      plugins <- forAll $ Gen.list (Range.linear 0 5) genPlugin
      let result = collectPackages Osx plugins
          allOsx = concatMap (\p -> _pluginOsx p ++ _pluginCask p) plugins
      assert $ all (`elem` allOsx) result

    it "UnsupportedDistro returns empty" $ hedgehog $ do
      plugins <- forAll $ Gen.list (Range.linear 0 5) genPlugin
      collectPackages UnsupportedDistro plugins === []

    it "no duplicates in result" $ hedgehog $ do
      plugins <- forAll $ Gen.list (Range.linear 0 5) genPlugin
      dist <- forAll $ Gen.element [Arch, Osx]
      let result = collectPackages dist plugins
      result === nub result

  describe "collectCaskPackages" $ do
    it "extracts only cask field" $ hedgehog $ do
      plugins <- forAll $ Gen.list (Range.linear 0 5) genPlugin
      let result = collectCaskPackages plugins
          allCask = concatMap _pluginCask plugins
      assert $ all (`elem` allCask) result

  describe "filterUninstalled" $ do
    it "never returns installed items" $ hedgehog $ do
      installed <- forAll $ Gen.list (Range.linear 0 10) genPkgName
      allPkgs <- forAll $ Gen.list (Range.linear 0 10) genPkgName
      let result = filterUninstalled installed allPkgs
      assert $ all (`notElem` installed) result

    it "returns all when nothing installed" $ do
      filterUninstalled [] ["a", "b", "c"] `shouldBe` ["a", "b", "c"]

    it "returns empty when all installed" $ do
      filterUninstalled ["a", "b"] ["a", "b"] `shouldBe` []

    it "matches tap-style names against installed formula name" $ do
      filterUninstalled ["taproom", "models"] ["gromgit/brewtils/taproom", "arimxyer/tap/models"]
        `shouldBe` []

    it "returns tap-style names when formula not installed" $ do
      filterUninstalled ["other"] ["gromgit/brewtils/taproom"]
        `shouldBe` ["gromgit/brewtils/taproom"]
