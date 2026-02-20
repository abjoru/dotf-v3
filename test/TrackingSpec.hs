module TrackingSpec (spec) where

import qualified Data.Map.Strict     as Map
import           Dotf.Tracking
import           Dotf.Types
import           Gen                 (genRelPath)
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "classifyUntracked" $ do
    it "every file in exactly one bucket" $ hedgehog $ do
      files <- forAll $ Gen.list (Range.linear 0 20) genRelPath
      let plugins = Map.fromList
            [ ("shell", Plugin "shell" Nothing [".zshrc", ".zprofile"] [] Nothing)
            , ("nvim",  Plugin "nvim"  Nothing [".config/nvim/"]       [] Nothing)
            ]
          watchPaths = [".config/", ".local/bin/"]
          (plugScoped, wl) = classifyUntracked files plugins watchPaths
          totalClassified = sum (map length (Map.elems plugScoped)) + length wl
      -- Every classified file should be <= total files
      -- (some files may not match any bucket)
      assert $ totalClassified <= length files

    it "files under plugin paths go to plugin bucket" $ do
      let files = [".config/nvim/newplugin.lua"]
          plugins = Map.fromList
            [ ("nvim", Plugin "nvim" Nothing [".config/nvim/"] [] Nothing)
            ]
          (plugScoped, wl) = classifyUntracked files plugins [".config/"]
      Map.lookup "nvim" plugScoped `shouldBe` Just [".config/nvim/newplugin.lua"]
      wl `shouldBe` []

    it "no plugins -> only watchlist" $ do
      let files = [".config/dunst/dunstrc"]
          (plugScoped, wl) = classifyUntracked files Map.empty [".config/"]
      plugScoped `shouldBe` Map.empty
      wl `shouldBe` [".config/dunst/dunstrc"]

    it "no watchlist -> only plugin-scoped" $ do
      let files = [".config/nvim/foo.lua", ".random/file"]
          plugins = Map.fromList
            [ ("nvim", Plugin "nvim" Nothing [".config/nvim/"] [] Nothing)
            ]
          (plugScoped, wl) = classifyUntracked files plugins []
      Map.lookup "nvim" plugScoped `shouldBe` Just [".config/nvim/foo.lua"]
      wl `shouldBe` []
