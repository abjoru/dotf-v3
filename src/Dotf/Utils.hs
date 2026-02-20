module Dotf.Utils (
  -- * Git path builders
  gitDirArg,
  workTreeArg,

  -- * Path constants
  dotfGitDir,
  gitIgnoreFile,
  metadataDir,
  pluginsFile,
  profilesFile,
  hooksDir,
  stateDir,
  stateFile,

  -- * IO utilities
  editFile,
  appendToFile,
  which,
  ask,
  ask',
) where

import           Control.Exception   (SomeException, try)
import           Data.String.Interpolate (i)
import           Dotf.Types          (GitEnv (..))
import           System.Directory    (doesFileExist)
import           System.FilePath     ((</>))
import           System.IO           (IOMode (WriteMode), hFlush, stdout,
                                      withFile)
import           System.Process      (readProcess)

-----------------------
-- Git path builders --
-----------------------

-- | Git dir argument for bare repo commands.
gitDirArg :: GitEnv -> String
gitDirArg env = [i|--git-dir=#{_geHome env </> ".dotf"}|]

-- | Git work-tree argument.
workTreeArg :: GitEnv -> String
workTreeArg env = [i|--work-tree=#{_geHome env}|]

--------------------
-- Path constants --
--------------------

-- | The bare repo git directory relative to home.
dotfGitDir :: GitEnv -> FilePath
dotfGitDir env = _geHome env </> ".dotf"

-- | Path to .gitignore.
gitIgnoreFile :: GitEnv -> FilePath
gitIgnoreFile env = _geHome env </> ".gitignore"

-- | Metadata directory (tracked in repo).
metadataDir :: GitEnv -> FilePath
metadataDir env = _geHome env </> ".config" </> "dotf"

-- | Path to plugins.yaml.
pluginsFile :: GitEnv -> FilePath
pluginsFile env = metadataDir env </> "plugins.yaml"

-- | Path to profiles.yaml.
profilesFile :: GitEnv -> FilePath
profilesFile env = metadataDir env </> "profiles.yaml"

-- | Path to hooks directory.
hooksDir :: GitEnv -> FilePath
hooksDir env = metadataDir env </> "hooks"

-- | Local state directory.
stateDir :: GitEnv -> FilePath
stateDir env = _geHome env </> ".local" </> "state" </> "dotf"

-- | Local state file.
stateFile :: GitEnv -> FilePath
stateFile env = stateDir env </> "state.yaml"

--------------------
-- IO Utilities   --
--------------------

-- | Open $EDITOR with the given file.
editFile :: FilePath -> IO ()
editFile file = do
  editor <- lookupEditor
  _ <- readProcess editor [file] ""
  pure ()
  where
    lookupEditor = pure "nvim" -- TODO: respect $EDITOR

-- | Append a line to a file, creating it if needed.
appendToFile :: String -> FilePath -> IO ()
appendToFile line file = do
  exists <- doesFileExist file
  if exists
    then appendFile file (line ++ "\n")
    else do
      withFile file WriteMode (\_ -> pure ())
      appendFile file (line ++ "\n")

-- | Check if a program exists on PATH.
which :: String -> IO Bool
which cmd = do
  rs <- try (readProcess "which" [cmd] "") :: IO (Either SomeException String)
  case rs of
    Left _  -> pure False
    Right o -> pure $ not $ null o

-- | Ask user a yes/no question.
ask :: String -> IO Bool
ask msg = do
  putStr msg
  hFlush stdout
  line <- getLine
  pure $ line `elem` ["y", "Y", "yes", "Yes"]

-- | Ask with default message.
ask' :: String -> IO Bool
ask' = ask
