module PluginSpec (spec) where

import qualified Data.Map.Strict     as Map
import           Dotf.Plugin
import           Dotf.Types
import           Hedgehog
import           Test.Hspec
import           Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "resolveDependencies" $ do
    it "includes all transitive deps" $ do
      let plugins = Map.fromList
            [ ("a", Plugin "a" Nothing [] ["b"] Nothing)
            , ("b", Plugin "b" Nothing [] ["c"] Nothing)
            , ("c", Plugin "c" Nothing [] []    Nothing)
            ]
      case resolveDependencies plugins ["a"] of
        Left err -> expectationFailure $ show err
        Right resolved -> do
          resolved `shouldContain` ["a"]
          resolved `shouldContain` ["b"]
          resolved `shouldContain` ["c"]

    it "respects dependency order" $ do
      let plugins = Map.fromList
            [ ("a", Plugin "a" Nothing [] ["b"] Nothing)
            , ("b", Plugin "b" Nothing [] []    Nothing)
            ]
      case resolveDependencies plugins ["a"] of
        Left err -> expectationFailure $ show err
        Right resolved -> do
          let idxB = elemIndex' "b" resolved
              idxA = elemIndex' "a" resolved
          idxB < idxA `shouldBe` True

    it "detects cycles" $ do
      let plugins = Map.fromList
            [ ("a", Plugin "a" Nothing [] ["b"] Nothing)
            , ("b", Plugin "b" Nothing [] ["a"] Nothing)
            ]
      case resolveDependencies plugins ["a"] of
        Left (DependencyError _) -> pure ()
        other -> expectationFailure $ "Expected DependencyError, got: " ++ show other

    it "detects missing deps" $ do
      let plugins = Map.fromList
            [ ("a", Plugin "a" Nothing [] ["missing"] Nothing)
            ]
      case resolveDependencies plugins ["a"] of
        Left (PluginNotFound _) -> pure ()
        other -> expectationFailure $ "Expected PluginNotFound, got: " ++ show other

  describe "checkRemoveSafety" $ do
    it "blocks removal when dependents exist" $ do
      let plugins = Map.fromList
            [ ("a", Plugin "a" Nothing [] ["b"] Nothing)
            , ("b", Plugin "b" Nothing [] []    Nothing)
            ]
      case checkRemoveSafety plugins ["a", "b"] ["b"] of
        Left (DependencyError _) -> pure ()
        other -> expectationFailure $ "Expected DependencyError, got: " ++ show other

    it "allows removal when no dependents" $ do
      let plugins = Map.fromList
            [ ("a", Plugin "a" Nothing [] ["b"] Nothing)
            , ("b", Plugin "b" Nothing [] []    Nothing)
            ]
      checkRemoveSafety plugins ["a", "b"] ["a"] `shouldBe` Right ()

  describe "validatePaths" $ do
    it "passes clean config" $ do
      let plugins = Map.fromList
            [ ("a", Plugin "a" Nothing [".zshrc"] [] Nothing)
            , ("b", Plugin "b" Nothing [".gitconfig"] [] Nothing)
            ]
      validatePaths plugins `shouldBe` Right ()

    it "rejects duplicate paths" $ hedgehog $ do
      let plugins = Map.fromList
            [ ("a", Plugin "a" Nothing [".zshrc"] [] Nothing)
            , ("b", Plugin "b" Nothing [".zshrc"] [] Nothing)
            ]
      case validatePaths plugins of
        Left (PathConflict _ _ _) -> success
        other                     -> annotateShow other >> failure

elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' _ [] = -1
elemIndex' x (y:ys)
  | x == y    = 0
  | otherwise = 1 + elemIndex' x ys
