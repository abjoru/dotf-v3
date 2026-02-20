module Main (main) where

import           Dotf.Commands
import           Dotf.Git         (hasBareRepo)
import           Dotf.Options
import           Dotf.Templates   (missingRepoMessage)
import           Dotf.Tui         (tui)
import           Dotf.Types       (GitEnv (..))
import           System.Directory (getHomeDirectory)
import           System.Exit      (exitFailure)

main :: IO ()
main = do
  home <- getHomeDirectory
  let env = GitEnv home
  mCmd <- readOptsOrTui
  case mCmd of
    Nothing  -> launchTui env
    Just cmd -> dispatch env cmd

launchTui :: GitEnv -> IO ()
launchTui env = do
  exists <- hasBareRepo env
  if not exists
    then putStrLn missingRepoMessage >> exitFailure
    else tui env

dispatch :: GitEnv -> Command -> IO ()
-- Setup commands (don't require existing repo)
dispatch env (Init url mProfile) = runInit env url mProfile
dispatch env New                 = runNew env
dispatch env Migrate             = runMigrate env

-- All other commands require existing bare repo
dispatch env cmd = do
  exists <- hasBareRepo env
  if not exists
    then putStrLn missingRepoMessage >> exitFailure
    else dispatchWithRepo env cmd

dispatchWithRepo :: GitEnv -> Command -> IO ()
-- Plugins
dispatchWithRepo env (PluginCmd PluginList)            = runPluginList env
dispatchWithRepo env (PluginCmd (PluginNew n d))       = runPluginNew env n d
dispatchWithRepo env (PluginCmd (PluginDelete n))      = runPluginDelete env n
dispatchWithRepo env (PluginCmd (PluginInstall ns))    = runPluginInstall env ns
dispatchWithRepo env (PluginCmd (PluginRemove ns))     = runPluginRemove env ns
dispatchWithRepo env (PluginCmd (PluginInfo n))        = runPluginInfo env n

-- Profiles
dispatchWithRepo env (ProfileCmd ProfileList)          = runProfileList env
dispatchWithRepo env (ProfileCmd (ProfileNew n ps))    = runProfileNew env n ps
dispatchWithRepo env (ProfileCmd (ProfileDelete n))    = runProfileDelete env n
dispatchWithRepo env (ProfileCmd (ProfileActivate n))  = runProfileActivate env n
dispatchWithRepo env (ProfileCmd ProfileDeactivate)    = runProfileDeactivate env
dispatchWithRepo env (ProfileCmd ProfileShow)          = runProfileShow env

-- Tracking
dispatchWithRepo env (Track path mPlugin)              = runTrack env path mPlugin
dispatchWithRepo env (Untrack path)                    = runUntrack env path
dispatchWithRepo env (Ignore pattern')                 = runIgnore env pattern'
dispatchWithRepo env (Untracked mPlugin)               = runUntracked env mPlugin

-- Watchlist
dispatchWithRepo env (WatchlistCmd (WatchlistAdd p))   = runWatchlistAdd env p
dispatchWithRepo env (WatchlistCmd (WatchlistRemove p))= runWatchlistRemove env p
dispatchWithRepo env (WatchlistCmd WatchlistList)      = runWatchlistList env

-- Save + manual git
dispatchWithRepo env (Save mMsg)                       = runSave env mMsg
dispatchWithRepo env (Stage files)                     = runStage env files
dispatchWithRepo env (Unstage files)                   = runUnstage env files
dispatchWithRepo env (Commit mMsg)                     = runCommit env mMsg
dispatchWithRepo env Push                              = runPush env
dispatchWithRepo env Pull                              = runPull env
dispatchWithRepo env Status                            = runStatus env
dispatchWithRepo env Diff                              = runDiff env
dispatchWithRepo env (GitRaw args)                     = runGitRaw env args

-- Catch-all (shouldn't happen with exhaustive patterns)
dispatchWithRepo _ _ = putStrLn "Unknown command" >> exitFailure
