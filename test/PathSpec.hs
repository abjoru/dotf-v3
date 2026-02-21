module PathSpec (spec) where

import qualified Data.Map.Strict     as Map
import           Dotf.Path
import           Dotf.Types
import           Gen                 (genRelPath)
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
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

    it "does not match prefix of different path component" $
      isSubpathOf ".config/nvim" ".config/nvimrc" `shouldBe` False

    it "child is not subpath of parent" $
      isSubpathOf ".config/nvim/init.lua" ".config/nvim" `shouldBe` False

  describe "findMatchingPlugin" $ do
    it "finds correct plugin" $ do
      let plugins = Map.fromList
            [ ("neovim", Plugin "neovim" Nothing [".config/nvim/"] [] Nothing [] [] [])
            , ("shell",  Plugin "shell"  Nothing [".zshrc", ".zprofile"] [] Nothing [] [] [])
            ]
      findMatchingPlugin ".config/nvim/init.lua" plugins `shouldBe` Just "neovim"

    it "returns Nothing when no match" $ do
      let plugins = Map.fromList
            [ ("shell", Plugin "shell" Nothing [".zshrc"] [] Nothing [] [] [])
            ]
      findMatchingPlugin ".config/kitty/kitty.conf" plugins `shouldBe` Nothing

    it "empty plugin map returns Nothing" $
      findMatchingPlugin ".zshrc" Map.empty `shouldBe` Nothing

  describe "consolidatePluginPaths" $ do
    let mkPlugin n ps = Plugin n Nothing ps [] Nothing [] [] []

    it "consolidates siblings with no conflict" $ do
      let plugins = Map.fromList
            [ ("neovim", mkPlugin "neovim" [".config/nvim/init.lua", ".config/nvim/lua/"])
            ]
      consolidatePluginPaths plugins [] `shouldBe`
        [("neovim", [".config/nvim/init.lua", ".config/nvim/lua/"], [".config/nvim"])]

    it "blocks consolidation when another plugin overlaps parent" $ do
      let plugins = Map.fromList
            [ ("neovim", mkPlugin "neovim" [".config/nvim/init.lua", ".config/nvim/lua/"])
            , ("other",  mkPlugin "other"  [".config/nvim/colors/"])
            ]
      consolidatePluginPaths plugins [] `shouldBe` []

    it "blocks consolidation when watchlist overlaps parent" $ do
      let plugins = Map.fromList
            [ ("neovim", mkPlugin "neovim" [".config/nvim/init.lua", ".config/nvim/lua/"])
            ]
          watchPaths = [".config/nvim/after/"]
      consolidatePluginPaths plugins watchPaths `shouldBe` []

    it "no-op for already consolidated paths" $ do
      let plugins = Map.fromList
            [ ("neovim", mkPlugin "neovim" [".config/nvim/"])
            ]
      consolidatePluginPaths plugins [] `shouldBe` []

    it "multi-level consolidation (iterative)" $ do
      let plugins = Map.fromList
            [ ("x", mkPlugin "x"
                [ ".config/app/a/file1"
                , ".config/app/a/file2"
                , ".config/app/b/file1"
                , ".config/app/b/file2"
                ])
            ]
      consolidatePluginPaths plugins [] `shouldBe`
        [("x", [".config/app/a/file1", ".config/app/a/file2"
               , ".config/app/b/file1", ".config/app/b/file2"]
             , [".config/app"])]

    it "deduplicates subsumed paths" $ do
      let plugins = Map.fromList
            [ ("s", mkPlugin "s" [".config/nvim/", ".config/nvim/init.lua"])
            ]
      consolidatePluginPaths plugins [] `shouldBe`
        [("s", [".config/nvim/", ".config/nvim/init.lua"], [".config/nvim/"])]

    it "single file not consolidated" $ do
      let plugins = Map.fromList
            [ ("s", mkPlugin "s" [".zshrc"])
            ]
      consolidatePluginPaths plugins [] `shouldBe` []

    it "never produces fewer paths than 1 per plugin" $ hedgehog $ do
      paths <- forAll $ Gen.list (Range.linear 1 6) genRelPath
      let plugins = Map.singleton "test" (mkPlugin "test" paths)
          result  = consolidatePluginPaths plugins []
      case result of
        []             -> success  -- no changes means original kept
        [(_, _, new')] -> assert (not (null new'))
        _              -> failure  -- only one plugin, max one entry
