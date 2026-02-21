module PathSpec (spec) where

import qualified Data.Map.Strict     as Map
import           Dotf.Path
import           Dotf.Types
import           Gen                 (genRelPath)
import           Hedgehog
import           Test.Hspec
import           Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "normalizePath" $ do
    it "idempotent" $ hedgehog $ do
      home <- forAll $ pure "/home/test"
      p <- forAll genRelPath
      let once  = normalizePath home p
          twice = normalizePath home once
      once === twice

    it "round-trip via relToAbs" $ hedgehog $ do
      home <- forAll $ pure "/home/test"
      p <- forAll genRelPath
      let normalized = normalizePath home p
          absolute   = relToAbs home normalized
          back       = normalizePath home absolute
      normalized === back

    it "handles absolute path" $
      normalizePath "/home/user" "/home/user/.zshrc" `shouldBe` ".zshrc"

    it "handles ~/ prefix" $
      normalizePath "/home/user" "~/.zshrc" `shouldBe` ".zshrc"

    it "handles relative path" $
      normalizePath "/home/user" ".config/nvim/init.lua"
        `shouldBe` ".config/nvim/init.lua"

  describe "isSubpathOf" $ do
    it "nested paths" $
      isSubpathOf ".config/nvim" ".config/nvim/init.lua" `shouldBe` True

    it "non-nested paths" $
      isSubpathOf ".config/nvim" ".config/kitty/kitty.conf" `shouldBe` False

    it "exact match" $
      isSubpathOf ".zshrc" ".zshrc" `shouldBe` True

    it "trailing slash parent" $
      isSubpathOf ".config/nvim/" ".config/nvim/init.lua" `shouldBe` True

  describe "findMatchingPlugin" $ do
    it "finds correct plugin" $ do
      let plugins = Map.fromList
            [ ("neovim", Plugin "neovim" Nothing [".config/nvim/"] [] Nothing)
            , ("shell",  Plugin "shell"  Nothing [".zshrc", ".zprofile"] [] Nothing)
            ]
      findMatchingPlugin ".config/nvim/init.lua" plugins `shouldBe` Just "neovim"

    it "returns Nothing when no match" $ do
      let plugins = Map.fromList
            [ ("shell", Plugin "shell" Nothing [".zshrc"] [] Nothing)
            ]
      findMatchingPlugin ".config/kitty/kitty.conf" plugins `shouldBe` Nothing

    it "empty plugin map returns Nothing" $
      findMatchingPlugin ".zshrc" Map.empty `shouldBe` Nothing
