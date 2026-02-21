module Dotf.Profile (
  -- * Pure
  checkCoverage,
  listProfiles,

  -- * IO
  showActiveProfile,
  profileCoverage,
  createProfile,
  deleteProfile,
  activateProfile,
  deactivateProfile,
) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import           Dotf.Config
import           Dotf.Git
import           Dotf.Path       (isSubpathOf)
import           Dotf.Plugin     (managedPaths, resolveDependencies)
import           Dotf.State
import           Dotf.Types

-- | Check file coverage: which tracked files are assigned to plugins, which aren't.
-- Returns (assigned, unassigned).
checkCoverage :: [RelPath] -> Map.Map PluginName Plugin -> ([RelPath], [RelPath])
checkCoverage files plugins =
  let metaPrefix = ".config/dotf/"
      userFiles = filter (not . (metaPrefix `isSubpathOf`)) files
      allPluginPaths = concatMap _pluginPaths (Map.elems plugins)
      isAssigned f = any (`isSubpathOf` f) allPluginPaths
      (assigned, unassigned) = foldr classify ([], []) userFiles
      classify f (as, us)
        | isAssigned f = (f:as, us)
        | otherwise    = (as, f:us)
  in (assigned, unassigned)

-- | List all profiles with their active status.
listProfiles :: ProfileConfig -> LocalState -> [(Profile, Bool)]
listProfiles cfg st =
  [ (p, Just (_profileName p) == _lsActiveProfile st)
  | p <- Map.elems (_prfProfiles cfg)
  ]

-- | Show the currently active profile if any.
showActiveProfile :: ProfileConfig -> LocalState -> Maybe (Profile, [PluginName])
showActiveProfile cfg st =
  case _lsActiveProfile st of
    Nothing -> Nothing
    Just name -> case Map.lookup name (_prfProfiles cfg) of
      Nothing -> Nothing
      Just p  -> Just (p, _lsInstalledPlugins st)

-- | Get coverage stats: (assigned count, total count, unassigned files).
profileCoverage :: GitEnv -> PluginConfig -> IO (Either DotfError (Int, Int, [RelPath]))
profileCoverage env pcfg = do
  tracked <- gitTracked env
  case tracked of
    Left err -> pure $ Left err
    Right files -> do
      let (assigned, unassigned) = checkCoverage files (_pcPlugins pcfg)
          total = length assigned + length unassigned
      pure $ Right (length assigned, total, unassigned)

-- | Create a new profile.
createProfile :: GitEnv -> ProfileName -> [PluginName] -> IO (Either DotfError ())
createProfile env name pluginNames = do
  cfgResult <- loadProfileConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg ->
      if Map.member name (_prfProfiles cfg)
      then pure $ Left $ ValidationError $ T.concat ["Profile already exists: ", name]
      else do
        let newProfile = Profile name pluginNames
            newCfg = cfg { _prfProfiles = Map.insert name newProfile (_prfProfiles cfg) }
        saveProfileConfig env newCfg
        pure $ Right ()

-- | Delete a profile definition.
deleteProfile :: GitEnv -> ProfileName -> IO (Either DotfError ())
deleteProfile env name = do
  cfgResult <- loadProfileConfig env
  case cfgResult of
    Left err -> pure $ Left err
    Right cfg ->
      case Map.lookup name (_prfProfiles cfg) of
        Nothing -> pure $ Left $ ProfileNotFound name
        Just _  -> do
          let newCfg = cfg { _prfProfiles = Map.delete name (_prfProfiles cfg) }
          saveProfileConfig env newCfg
          stResult <- loadLocalState env
          case stResult of
            Left err -> pure $ Left err
            Right st -> do
              -- Clear active profile if we're deleting the active one
              case _lsActiveProfile st of
                Just active | active == name -> do
                  let newSt = st { _lsActiveProfile = Nothing }
                  saveLocalState env newSt
                _ -> pure ()
              pure $ Right ()

-- | Activate a profile: check coverage, resolve deps, set sparse checkout.
activateProfile :: GitEnv -> ProfileName -> IO (Either DotfError ())
activateProfile env name = do
  pcfgResult <- loadPluginConfig env
  prfResult  <- loadProfileConfig env
  case (pcfgResult, prfResult) of
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err
    (Right pcfg, Right prf) ->
      case Map.lookup name (_prfProfiles prf) of
        Nothing -> pure $ Left $ ProfileNotFound name
        Just profile -> do
          -- Check coverage — all tracked files must be assigned
          covResult <- profileCoverage env pcfg
          case covResult of
            Left err -> pure $ Left err
            Right (_, _, unassigned)
              | not (null unassigned) ->
                pure $ Left $ UnassignedFilesExist unassigned
              | otherwise -> do
                -- Resolve all dependencies for the profile's plugins
                case resolveDependencies (_pcPlugins pcfg) (_profilePlugins profile) of
                  Left err -> pure $ Left err
                  Right resolved -> do
                    let allPaths = concatMap (getPaths pcfg) resolved
                        sparseTargets = managedPaths ++ allPaths
                    initResult <- gitSparseCheckoutInit env
                    case initResult of
                      Left err -> pure $ Left err
                      Right () -> do
                        setResult <- gitSparseCheckoutSet env sparseTargets
                        case setResult of
                          Left err -> pure $ Left err
                          Right () -> do
                            let newSt = LocalState (Just name) resolved
                            saveLocalState env newSt
                            pure $ Right ()
  where
    getPaths pcfg plugName =
      case Map.lookup plugName (_pcPlugins pcfg) of
        Nothing -> []
        Just p  -> _pluginPaths p

-- | Deactivate profile: disable sparse checkout, clear state.
deactivateProfile :: GitEnv -> IO (Either DotfError ())
deactivateProfile env = do
  result <- gitSparseCheckoutDisable env
  case result of
    Left err -> pure $ Left err
    Right () -> do
      stResult <- loadLocalState env
      case stResult of
        Left err -> pure $ Left err
        Right st -> do
          let newSt = st { _lsActiveProfile = Nothing, _lsInstalledPlugins = [] }
          saveLocalState env newSt
          pure $ Right ()
