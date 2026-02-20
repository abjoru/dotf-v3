module Dotf.State (
  loadLocalState,
  saveLocalState,
  defaultLocalState,
) where

import qualified Data.Yaml       as Y
import           Dotf.Types
import           Dotf.Utils      (stateDir, stateFile)
import           System.Directory (createDirectoryIfMissing, doesFileExist)

-- | Load local state from ~/.local/state/dotf/state.yaml.
-- Returns default state if file doesn't exist.
loadLocalState :: GitEnv -> IO LocalState
loadLocalState env = do
  let path = stateFile env
  exists <- doesFileExist path
  if not exists
    then pure defaultLocalState
    else do
      result <- Y.decodeFileEither path
      case result of
        Left _    -> pure defaultLocalState
        Right st  -> pure st

-- | Save local state, creating directory if needed.
saveLocalState :: GitEnv -> LocalState -> IO ()
saveLocalState env st = do
  createDirectoryIfMissing True (stateDir env)
  Y.encodeFile (stateFile env) st

-- | Default empty state.
defaultLocalState :: LocalState
defaultLocalState = LocalState Nothing []
