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

  -- * Freeze (skip-worktree)
  gitFreeze,
  gitUnfreeze,
  gitListFrozen,

  -- * Queries
  hasBareRepo,
  hasMergeHead,
  gitConflictFiles,
  gitAhead,
  gitBehind,

  -- * Result processing
  processFileListResult,
  processStringResult,
  mapEitherM,
) where

import           Control.Exception          (SomeException, try)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Function              ((&))
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Text.Encoding.Error   (lenientDecode)
import           Dotf.Types
import           Dotf.Utils                 (dotfGitDir, gitDirArg, workTreeArg)
import           System.Directory           (doesDirectoryExist, doesFileExist,
                                             doesPathExist)
import           System.FilePath            ((</>))
import qualified System.Process.Typed       as PT

type ReadProcessResult = (PT.ExitCode, B.ByteString, B.ByteString)

---------------------
-- Core operations --
---------------------

-- | Run a git command on the bare repo, returning success/failure.
runGit :: GitEnv -> [String] -> IO (Either DotfError ())
runGit env args = do
  let cfg = gitBare env args
  (exit, _, err) <- PT.readProcess cfg
  case exit of
    PT.ExitSuccess   -> pure $ Right ()
    PT.ExitFailure c -> pure $ Left $ GitError c (decodeErr err)

-- | Run a git command and return stdout.
runGitOutput :: GitEnv -> [String] -> IO (Either DotfError String)
runGitOutput env args = do
  let cfg = gitBare env args
  res <- PT.readProcess cfg
  pure $ processStringResult res

-- | Build a process config for a bare repo git command.
gitBare :: GitEnv -> [String] -> PT.ProcessConfig () () ()
gitBare env args =
  let home = _geHome env
      newArgs = [gitDirArg env, workTreeArg env] ++ args
  in PT.setWorkingDir home (PT.proc "git" newArgs)

-- | Build a process config with output silenced.
gitBareSilent :: GitEnv -> [String] -> PT.ProcessConfig () () ()
gitBareSilent env args =
  gitBare env args
    & PT.setStdout PT.nullStream
    & PT.setStderr PT.nullStream

------------------
-- File listing --
------------------

-- | List all tracked files (index-based so track/untrack reflect immediately).
gitTracked :: GitEnv -> IO (Either DotfError [FilePath])
gitTracked env =
  processFileListResult <$> PT.readProcess (gitBare env ["ls-files"])

-- | List staged files.
gitTrackedStaged :: GitEnv -> IO (Either DotfError [FilePath])
gitTrackedStaged env =
  processFileListResult <$> PT.readProcess (gitBare env ["diff", "--name-only", "--cached"])

-- | List unstaged (modified) files.
gitTrackedUnstaged :: GitEnv -> IO (Either DotfError [FilePath])
gitTrackedUnstaged env =
  processFileListResult <$> PT.readProcess (gitBare env ["diff", "--name-only"])

-- | List untracked files, optionally scoped to specific paths.
-- When paths are provided, git only scans those directories (much faster
-- than scanning all of $HOME).
gitUntracked :: GitEnv -> [FilePath] -> IO (Either DotfError [FilePath])
gitUntracked env scopePaths =
  processFileListResult <$> PT.readProcess (gitBare env (["ls-files", "--exclude-standard", "--others"] ++ ["--" | not (null scopePaths)] ++ scopePaths))

-------------
-- Staging --
-------------

-- | Add a file to the index.
gitAdd :: GitEnv -> FilePath -> IO (Either DotfError ())
gitAdd env fp = runGit env ["add", "-f", fp]

-- | Remove a file from the index (keep on disk).
gitRmCached :: GitEnv -> FilePath -> IO (Either DotfError ())
gitRmCached env fp = runGit env ["rm", "--cached", fp]

-- | Stage a file: tries 'git add', then 'git rm -f' for deletions.
gitStage :: GitEnv -> FilePath -> IO (Either DotfError ())
gitStage env fp = do
  result <- gitAdd env fp
  case result of
    Right () -> pure (Right ())
    Left _   -> do
      rmResult <- runGit env ["rm", "-f", fp]
      case rmResult of
        Right () -> pure (Right ())
        Left _   -> pure result  -- return original add error

-- | Unstage a file. Falls back to 'rm --cached' if reset fails.
gitUnstage :: GitEnv -> FilePath -> IO (Either DotfError ())
gitUnstage env fp = do
  result <- runGit env ["reset", "--", fp]
  case result of
    Right () -> pure (Right ())
    Left _   -> runGit env ["rm", "--cached", fp]

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
      cfg    = PT.proc "git" ["clone", "--bare", url, gitDir]
  (exit, _, err) <- PT.readProcess cfg
  case exit of
    PT.ExitSuccess   -> runGit env ["checkout"]
    PT.ExitFailure c -> pure $ Left $ GitError c (decodeErr err)

-- | Initialize a new bare repo.
gitInitBare :: GitEnv -> IO (Either DotfError ())
gitInitBare env = do
  let gitDir = dotfGitDir env
  let cfg = PT.proc "git" ["init", "--bare", gitDir]
  (exit, _, err) <- PT.readProcess cfg
  case exit of
    PT.ExitSuccess   -> pure $ Right ()
    PT.ExitFailure c -> pure $ Left $ GitError c (decodeErr err)

---------------------
-- Sparse checkout --
---------------------

-- | Initialize sparse checkout.
gitSparseCheckoutInit :: GitEnv -> IO (Either DotfError ())
gitSparseCheckoutInit env = runGit env ["sparse-checkout", "init", "--no-cone"]

-- | Set sparse checkout patterns (replaces existing).
gitSparseCheckoutSet :: GitEnv -> [FilePath] -> IO (Either DotfError ())
gitSparseCheckoutSet env paths = runGit env (["sparse-checkout", "set", "--no-cone"] ++ paths)

-- | Add paths to sparse checkout (read-modify-write via set --no-cone).
-- git 2.53+ doesn't accept --no-cone on 'add', so we read current patterns,
-- append new ones, and 'set --no-cone' the full list.
gitSparseCheckoutAdd :: GitEnv -> [FilePath] -> IO (Either DotfError ())
gitSparseCheckoutAdd env paths = do
  current <- gitSparseCheckoutList env
  case current of
    Left err -> pure $ Left err
    Right existing ->
      let merged = existing ++ filter (`notElem` existing) paths
      in gitSparseCheckoutSet env merged

-- | List current sparse checkout patterns.
gitSparseCheckoutList :: GitEnv -> IO (Either DotfError [FilePath])
gitSparseCheckoutList env =
  processFileListResult <$> PT.readProcess (gitBare env ["sparse-checkout", "list"])

-- | Disable sparse checkout (full checkout).
gitSparseCheckoutDisable :: GitEnv -> IO (Either DotfError ())
gitSparseCheckoutDisable env = runGit env ["sparse-checkout", "disable"]

-------------------
-- Status + diff --
-------------------

-- | Get git status, optionally scoped to specific paths.
gitStatus :: GitEnv -> [FilePath] -> IO (Either DotfError String)
gitStatus env paths = runGitOutput env (["status", "-sb"] ++ ["--" | not (null paths)] ++ paths)

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
  let cfg = gitBare env args
  (exit, _, err) <- PT.readProcess cfg
  case exit of
    PT.ExitSuccess   -> pure $ Right ()
    PT.ExitFailure c -> pure $ Left $ GitError c (decodeErr err)

--------------------------
-- Freeze (skip-worktree)
--------------------------

-- | Freeze a file (skip-worktree).
gitFreeze :: GitEnv -> FilePath -> IO (Either DotfError ())
gitFreeze env fp = runGit env ["update-index", "--skip-worktree", fp]

-- | Unfreeze a file (no-skip-worktree).
gitUnfreeze :: GitEnv -> FilePath -> IO (Either DotfError ())
gitUnfreeze env fp = runGit env ["update-index", "--no-skip-worktree", fp]

-- | List all frozen (skip-worktree) files.
gitListFrozen :: GitEnv -> IO (Either DotfError [FilePath])
gitListFrozen env = do
  let cfg = gitBare env ["ls-files", "-v"]
  (exit, out, err) <- PT.readProcess cfg
  case exit of
    PT.ExitFailure c -> pure $ Left $ GitError c (decodeErr err)
    PT.ExitSuccess   -> pure $ Right
      [ drop 2 line'
      | line' <- lines (C8.unpack out)
      , take 2 line' == "S "
      ]

-------------
-- Queries --
-------------

-- | Check if a bare repo exists.
hasBareRepo :: GitEnv -> IO Bool
hasBareRepo env = doesDirectoryExist (dotfGitDir env)

-- | Check if a merge is in progress.
hasMergeHead :: GitEnv -> IO Bool
hasMergeHead env = doesFileExist (dotfGitDir env </> "MERGE_HEAD")

-- | List files with merge conflicts.
gitConflictFiles :: GitEnv -> IO (Either DotfError [FilePath])
gitConflictFiles env =
  processFileListResult <$> PT.readProcess (gitBare env ["diff", "--name-only", "--diff-filter=U"])

-- | Get unpushed-commits count relative to upstream.
-- Falls back to 0 on any error (no upstream, detached HEAD, etc.).
gitAhead :: GitEnv -> IO Int
gitAhead env = do
  result <- try go :: IO (Either SomeException Int)
  pure $ either (const 0) id result
  where
    go = do
      let cfg = gitBare env ["rev-list", "--count", "@{upstream}..HEAD"]
      (exit, out, _) <- PT.readProcess cfg
      case exit of
        PT.ExitFailure _ -> pure 0
        PT.ExitSuccess   -> pure $ readDef 0 (C8.unpack out)
    readDef d s = case reads (filter (/= '\n') s) of
      [(n, "")] -> n
      _         -> d

-- | Get commits-behind count relative to upstream.
-- Falls back to 0 on any error (no upstream, detached HEAD, etc.).
gitBehind :: GitEnv -> IO Int
gitBehind env = do
  result <- try go :: IO (Either SomeException Int)
  pure $ either (const 0) id result
  where
    go = do
      let cfg = gitBare env ["rev-list", "--count", "HEAD..@{upstream}"]
      (exit, out, _) <- PT.readProcess cfg
      case exit of
        PT.ExitFailure _ -> pure 0
        PT.ExitSuccess   -> pure $ readDef 0 (C8.unpack out)
    readDef d s = case reads (filter (/= '\n') s) of
      [(n, "")] -> n
      _         -> d

-----------
-- Utils --
-----------

-- | Decode a lazy ByteString to Text, replacing invalid UTF-8 sequences.
decodeErr :: B.ByteString -> Text
decodeErr = TE.decodeUtf8With lenientDecode . B.toStrict

-----------------------
-- Result processing --
-----------------------

-- | Process a git command result into a list of file paths.
processFileListResult :: ReadProcessResult -> Either DotfError [FilePath]
processFileListResult (PT.ExitFailure c, _, err) =
  Left $ GitError c (decodeErr err)
processFileListResult (PT.ExitSuccess, out, _) =
  Right $ filter (not . null) $ map C8.unpack (C8.lines out)

-- | Process a git command result into a string.
processStringResult :: ReadProcessResult -> Either DotfError String
processStringResult (PT.ExitFailure c, _, err) =
  Left $ GitError c (decodeErr err)
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
