module OptionsSpec (spec) where

import           Dotf.Options
import           Options.Applicative
import           Test.Hspec

spec :: Spec
spec = do
  describe "CLI parser" $ do
    it "parses 'plugin list'" $
      parseArgs ["plugin", "list"] `shouldSatisfy` isPluginList

    it "parses 'profile activate foo'" $
      parseArgs ["profile", "activate", "foo"] `shouldSatisfy` isProfileActivate

    it "parses 'track file --plugin name'" $
      parseArgs ["track", ".zshrc", "--plugin", "shell"] `shouldSatisfy` isTrackWithPlugin

    it "parses 'save' with message" $
      parseArgs ["save", "initial commit"] `shouldSatisfy` isSaveWithMsg

    it "parses 'git' passthrough" $
      parseArgs ["git", "log"] `shouldSatisfy` isGitRaw

    it "parses 'init' with profile" $
      parseArgs ["init", "git@github.com:user/dotf.git", "--profile", "work"]
        `shouldSatisfy` isInitWithProfile

    it "parses 'plugin install' multiple names" $
      parseArgs ["plugin", "install", "shell", "git"] `shouldSatisfy` isPluginInstallMulti

    it "parses 'watchlist add'" $
      parseArgs ["watchlist", "add", ".config/"] `shouldSatisfy` isWatchlistAdd

    it "parses 'status'" $
      parseArgs ["status"] `shouldSatisfy` isStatus

-- Parse helper using exported commandParser
parseArgs :: [String] -> Maybe Command
parseArgs args =
  getParseResult $ execParserPure defaultPrefs parser args
  where
    parser = info (commandParser <**> helper) fullDesc

-- Predicates
isPluginList :: Maybe Command -> Bool
isPluginList (Just (PluginCmd PluginList)) = True
isPluginList _ = False

isProfileActivate :: Maybe Command -> Bool
isProfileActivate (Just (ProfileCmd (ProfileActivate _))) = True
isProfileActivate _ = False

isTrackWithPlugin :: Maybe Command -> Bool
isTrackWithPlugin (Just (Track _ (Just _))) = True
isTrackWithPlugin _ = False

isSaveWithMsg :: Maybe Command -> Bool
isSaveWithMsg (Just (Save (Just _))) = True
isSaveWithMsg _ = False

isGitRaw :: Maybe Command -> Bool
isGitRaw (Just (GitRaw _)) = True
isGitRaw _ = False

isInitWithProfile :: Maybe Command -> Bool
isInitWithProfile (Just (Init _ (Just _))) = True
isInitWithProfile _ = False

isPluginInstallMulti :: Maybe Command -> Bool
isPluginInstallMulti (Just (PluginCmd (PluginInstall xs))) = length xs > 1
isPluginInstallMulti _ = False

isWatchlistAdd :: Maybe Command -> Bool
isWatchlistAdd (Just (WatchlistCmd (WatchlistAdd _))) = True
isWatchlistAdd _ = False

isStatus :: Maybe Command -> Bool
isStatus (Just Status) = True
isStatus _ = False
