module Dotf.Git (
  -- * Core git operations
  runGit,
  runGitOutput,
  gitBare,
  gitBareSilent,

  -- * File listing
  gitTracked,
  gitTrackedStaged,
  gitTrackedUnstaged,
  gitUntracked,

  -- * Staging
  gitAdd,
  gitRmCached,
  gitStage,
  gitUnstage,

  -- * Commits + sync
  gitCommit,
  gitPush,
  gitPull,

  -- * Repo init
  gitCloneBare,
  gitInitBare,

  -- * Sparse checkout
  gitSparseCheckoutInit,
  gitSparseCheckoutSet,
  gitSparseCheckoutAdd,
  gitSparseCheckoutList,
  gitSparseCheckoutDisable,

  -- * Status + diff
  gitStatus,
  gitDiff,
  gitDiffFile,

  -- * Passthrough
  gitRaw,

  -- * Queries
  hasBareRepo,
  gitAheadBehind,

  -- * Result processing
  processFileListResult,
  processStringResult,
  mapEitherM,
) where

import           Control.Exception          (SomeException, try)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Function              ((&))
import           Data.Text                  (pack)
import           Dotf.Types
import           Dotf.Utils                 (dotfGitDir, gitDirArg, workTreeArg)
import           System.Directory           (doesDirectoryExist, doesPathExist)
import           System.FilePath            ((</>))
import           System.IO                  (IOMode (WriteMode), openFile)
import qualified System.Process.Typed       as PT

type ReadProcessResult = (PT.ExitCode, B.ByteString, B.ByteString)

---------------------
-- Core operations --
---------------------

-- | Run a git command on the bare repo, returning success/failure.
runGit :: GitEnv -> [String] -> IO (Either DotfError ())
runGit env args = do
  cfg <- gitBare env args
  (exit, _, err) <- PT.readProcess cfg
  case exit of
    PT.ExitSuccess   -> pure $ Right ()
    PT.ExitFailure c -> pure $ Left $ GitError c (pack $ C8.unpack err)

-- | Run a git command and return stdout.
runGitOutput :: GitEnv -> [String] -> IO (Either DotfError String)
runGitOutput env args = do
  cfg <- gitBare env args
  res <- PT.readProcess cfg
  pure $ processStringResult res

-- | Build a process config for a bare repo git command.
gitBare :: GitEnv -> [String] -> IO (PT.ProcessConfig () () ())
gitBare env args = do
  let home = _geHome env
      newArgs = [gitDirArg env, workTreeArg env] ++ args
  pure $ PT.setWorkingDir home (PT.proc "git" newArgs)

-- | Build a process config with output silenced.
gitBareSilent :: GitEnv -> [String] -> IO (PT.ProcessConfig () () ())
gitBareSilent env args = do
  cfg     <- gitBare env args
  devNull <- openFile "/dev/null" WriteMode
  pure $ cfg & PT.setStdout (PT.useHandleClose devNull)
             & PT.setStderr (PT.useHandleClose devNull)

------------------
-- File listing --
------------------

-- | List all tracked files.
gitTracked :: GitEnv -> IO (Either DotfError [FilePath])
gitTracked env = do
  cfg <- gitBare env ["ls-tree", "--name-only", "-r", "HEAD"]
  processFileListResult <$> PT.readProcess cfg

-- | List staged files.
gitTrackedStaged :: GitEnv -> IO (Either DotfError [FilePath])
gitTrackedStaged env = do
  cfg <- gitBare env ["diff", "--name-only", "--cached"]
  processFileListResult <$> PT.readProcess cfg

-- | List unstaged (modified) files.
gitTrackedUnstaged :: GitEnv -> IO (Either DotfError [FilePath])
gitTrackedUnstaged env = do
  cfg <- gitBare env ["diff", "--name-only"]
  processFileListResult <$> PT.readProcess cfg

-- | List untracked files.
gitUntracked :: GitEnv -> IO (Either DotfError [FilePath])
gitUntracked env = do
  cfg <- gitBare env ["ls-files", "--exclude-standard", "--others"]
  processFileListResult <$> PT.readProcess cfg

-------------
-- Staging --
-------------

-- | Add a file to the index.
gitAdd :: GitEnv -> FilePath -> IO (Either DotfError ())
gitAdd env fp = runGit env ["add", fp]

-- | Remove a file from the index (keep on disk).
gitRmCached :: GitEnv -> FilePath -> IO (Either DotfError ())
gitRmCached env fp = runGit env ["rm", "--cached", fp]

-- | Stage a file (alias for gitAdd).
gitStage :: GitEnv -> FilePath -> IO (Either DotfError ())
gitStage = gitAdd

-- | Unstage a file.
gitUnstage :: GitEnv -> FilePath -> IO (Either DotfError ())
gitUnstage env fp = runGit env ["reset", "--", fp]

--------------------
-- Commits + sync --
--------------------

-- | Commit with a message.
gitCommit :: GitEnv -> String -> IO (Either DotfError ())
gitCommit env msg = runGit env ["commit", "-m", msg]

-- | Push to remote.
gitPush :: GitEnv -> IO (Either DotfError ())
gitPush env = runGit env ["push"]

-- | Pull from remote.
gitPull :: GitEnv -> IO (Either DotfError ())
gitPull env = runGit env ["pull"]

---------------
-- Repo init --
---------------

-- | Clone a bare repo.
gitCloneBare :: GitEnv -> String -> IO (Either DotfError ())
gitCloneBare env url = do
  let gitDir = dotfGitDir env
  cfg <- pure $ PT.proc "git" ["clone", "--bare", url, gitDir]
  (exit, _, err) <- PT.readProcess cfg
  case exit of
    PT.ExitSuccess   -> runGit env ["checkout"]
    PT.ExitFailure c -> pure $ Left $ GitError c (pack $ C8.unpack err)

-- | Initialize a new bare repo.
gitInitBare :: GitEnv -> IO (Either DotfError ())
gitInitBare env = do
  let gitDir = dotfGitDir env
  let cfg = PT.proc "git" ["init", "--bare", gitDir]
  (exit, _, err) <- PT.readProcess cfg
  case exit of
    PT.ExitSuccess   -> pure $ Right ()
    PT.ExitFailure c -> pure $ Left $ GitError c (pack $ C8.unpack err)

---------------------
-- Sparse checkout --
---------------------

-- | Initialize sparse checkout.
gitSparseCheckoutInit :: GitEnv -> IO (Either DotfError ())
gitSparseCheckoutInit env = runGit env ["sparse-checkout", "init"]

-- | Set sparse checkout patterns (replaces existing).
gitSparseCheckoutSet :: GitEnv -> [FilePath] -> IO (Either DotfError ())
gitSparseCheckoutSet env paths = runGit env (["sparse-checkout", "set"] ++ paths)

-- | Add paths to sparse checkout.
gitSparseCheckoutAdd :: GitEnv -> [FilePath] -> IO (Either DotfError ())
gitSparseCheckoutAdd env paths = runGit env (["sparse-checkout", "add"] ++ paths)

-- | List current sparse checkout patterns.
gitSparseCheckoutList :: GitEnv -> IO (Either DotfError [FilePath])
gitSparseCheckoutList env = do
  cfg <- gitBare env ["sparse-checkout", "list"]
  processFileListResult <$> PT.readProcess cfg

-- | Disable sparse checkout (full checkout).
gitSparseCheckoutDisable :: GitEnv -> IO (Either DotfError ())
gitSparseCheckoutDisable env = runGit env ["sparse-checkout", "disable"]

-------------------
-- Status + diff --
-------------------

-- | Get git status.
gitStatus :: GitEnv -> IO (Either DotfError String)
gitStatus env = runGitOutput env ["status", "-sb"]

-- | Get full diff.
gitDiff :: GitEnv -> IO (Either DotfError String)
gitDiff env = runGitOutput env ["diff"]

-- | Get diff for a single file.
gitDiffFile :: GitEnv -> FilePath -> IO (Either DotfError String)
gitDiffFile env fp = runGitOutput env ["diff", fp]

-----------------
-- Passthrough --
-----------------

-- | Run arbitrary git command.
gitRaw :: GitEnv -> [String] -> IO (Either DotfError ())
gitRaw env args = do
  cfg <- gitBare env args
  (exit, _, err) <- PT.readProcess cfg
  case exit of
    PT.ExitSuccess   -> pure $ Right ()
    PT.ExitFailure c -> pure $ Left $ GitError c (pack $ C8.unpack err)

-------------
-- Queries --
-------------

-- | Check if a bare repo exists.
hasBareRepo :: GitEnv -> IO Bool
hasBareRepo env = doesDirectoryExist (dotfGitDir env)

-- | Get (ahead, behind) counts relative to upstream.
-- Falls back to (0,0) on any error.
gitAheadBehind :: GitEnv -> IO (Int, Int)
gitAheadBehind env = do
  result <- try go :: IO (Either SomeException (Int, Int))
  pure $ either (const (0, 0)) id result
  where
    go = do
      cfg <- gitBare env ["rev-list", "--left-right", "--count", "HEAD...@{upstream}"]
      (exit, out, _) <- PT.readProcess cfg
      case exit of
        PT.ExitFailure _ -> pure (0, 0)
        PT.ExitSuccess   -> pure $ parseCounts (C8.unpack out)
    parseCounts s = case words s of
      [a, b] -> (readDef 0 a, readDef 0 b)
      _      -> (0, 0)
    readDef d str' = case reads str' of
      [(n, "")] -> n
      _         -> d

-----------------------
-- Result processing --
-----------------------

-- | Process a git command result into a list of file paths.
processFileListResult :: ReadProcessResult -> Either DotfError [FilePath]
processFileListResult (PT.ExitFailure c, _, err) =
  Left $ GitError c (pack $ C8.unpack err)
processFileListResult (PT.ExitSuccess, out, _) =
  Right $ filter (not . null) $ map C8.unpack (C8.lines out)

-- | Process a git command result into a string.
processStringResult :: ReadProcessResult -> Either DotfError String
processStringResult (PT.ExitFailure c, _, err) =
  Left $ GitError c (pack $ C8.unpack err)
processStringResult (PT.ExitSuccess, out, _) =
  Right $ C8.unpack out

-- | Map an Either in IO, checking file existence.
mapEitherM :: (FilePath -> Bool -> TrackedType)
           -> Either DotfError [FilePath]
           -> GitEnv
           -> IO (Either DotfError [TrackedType])
mapEitherM _ (Left err) _     = pure $ Left err
mapEitherM f (Right vs) env = fmap Right $ mapM checkPath vs
  where
    checkPath p = f p <$> doesPathExist (_geHome env </> p)
