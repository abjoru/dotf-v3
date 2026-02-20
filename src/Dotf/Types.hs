module Dotf.Types (
  -- * Core environment
  GitEnv(..),

  -- * Type aliases
  PluginName,
  ProfileName,
  RelPath,

  -- * Plugin types
  Hook(..),
  Plugin(..),
  Watchlist(..),
  PluginConfig(..),

  -- * Profile types
  Profile(..),
  ProfileConfig(..),

  -- * State types
  LocalState(..),

  -- * Tracking types
  TrackedType(..),
  FileStatus(..),
  UntrackedReport(..),

  -- * Error types
  DotfError(..),

  -- * Lenses
  -- ** GitEnv
  geHome,
  -- ** Plugin
  pluginName, pluginDescription, pluginPaths, pluginDepends, pluginPostInstall,
  -- ** PluginConfig
  pcPlugins, pcWatchlist,
  -- ** Profile
  profileName, profilePlugins,
  -- ** ProfileConfig
  prfProfiles,
  -- ** LocalState
  lsActiveProfile, lsInstalledPlugins,
  -- ** UntrackedReport
  urPluginScoped, urWatchlist,
  -- ** Watchlist
  wlPaths,
) where

import           Data.Aeson       (FromJSON (..), ToJSON (..), Value (..),
                                   object, withObject, (.!=), (.:), (.:?), (.=))
import           Data.Aeson.Types (Parser)
import qualified Data.Map.Strict  as Map
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Lens.Micro.TH    (makeLenses)

-----------
-- Types --
-----------

type PluginName  = Text
type ProfileName = Text
type RelPath     = FilePath

data GitEnv = GitEnv
  { _geHome :: FilePath
  } deriving (Show, Eq)

data Hook
  = InlineHook [Text]
  | ScriptHook FilePath
  deriving (Show, Eq)

data Plugin = Plugin
  { _pluginName        :: PluginName
  , _pluginDescription :: Maybe Text
  , _pluginPaths       :: [RelPath]
  , _pluginDepends     :: [PluginName]
  , _pluginPostInstall :: Maybe Hook
  } deriving (Show, Eq)

newtype Watchlist = Watchlist
  { _wlPaths :: [RelPath]
  } deriving (Show, Eq)

data PluginConfig = PluginConfig
  { _pcPlugins   :: Map.Map PluginName Plugin
  , _pcWatchlist :: Watchlist
  } deriving (Show, Eq)

data Profile = Profile
  { _profileName    :: ProfileName
  , _profilePlugins :: [PluginName]
  } deriving (Show, Eq)

data ProfileConfig = ProfileConfig
  { _prfProfiles :: Map.Map ProfileName Profile
  } deriving (Show, Eq)

data LocalState = LocalState
  { _lsActiveProfile    :: Maybe ProfileName
  , _lsInstalledPlugins :: [PluginName]
  } deriving (Show, Eq)

data FileStatus = Exists | Deleted
  deriving (Show, Eq, Ord)

data TrackedType
  = Tracked  RelPath
  | Staged   RelPath FileStatus
  | Unstaged RelPath FileStatus
  deriving (Show, Eq)

data UntrackedReport = UntrackedReport
  { _urPluginScoped :: Map.Map PluginName [RelPath]
  , _urWatchlist    :: [RelPath]
  } deriving (Show, Eq)

data DotfError
  = GitError Int Text
  | ConfigError Text
  | PathConflict PluginName PluginName RelPath
  | PluginNotFound PluginName
  | ProfileNotFound ProfileName
  | DependencyError Text
  | UnassignedFilesExist [RelPath]
  | ValidationError Text
  deriving (Show, Eq)

------------
-- Lenses --
------------

makeLenses ''GitEnv
makeLenses ''Plugin
makeLenses ''Watchlist
makeLenses ''PluginConfig
makeLenses ''Profile
makeLenses ''ProfileConfig
makeLenses ''LocalState
makeLenses ''UntrackedReport

--------------------
-- JSON Instances --
--------------------

instance FromJSON Hook where
  parseJSON (Array arr) = InlineHook <$> mapM parseJSON (toList arr)
    where toList = foldr (:) []
  parseJSON (String s)  = pure $ ScriptHook (T.unpack s)
  parseJSON v           = fail $ "Expected Array or String for Hook, got: " ++ show v

instance ToJSON Hook where
  toJSON (InlineHook cmds) = toJSON cmds
  toJSON (ScriptHook path) = toJSON path

instance FromJSON Plugin where
  parseJSON = withObject "Plugin" $ \v ->
    Plugin <$> v .:  "name"
           <*> v .:? "description"
           <*> v .:? "paths" .!= []
           <*> v .:? "depends" .!= []
           <*> v .:? "post-install"

instance ToJSON Plugin where
  toJSON p = object
    [ "name"         .= _pluginName p
    , "description"  .= _pluginDescription p
    , "paths"        .= _pluginPaths p
    , "depends"      .= _pluginDepends p
    , "post-install" .= _pluginPostInstall p
    ]

instance FromJSON Watchlist where
  parseJSON (Array arr) = Watchlist <$> mapM parseJSON (toList arr)
    where toList = foldr (:) []
  parseJSON _           = fail "Expected Array for Watchlist"

instance ToJSON Watchlist where
  toJSON (Watchlist ps) = toJSON ps

-- | PluginConfig parses the top-level plugins.yaml structure:
-- @
-- plugins:
--   shell:
--     description: "..."
--     paths: [...]
-- watchlist:
--   - .config/
-- @
instance FromJSON PluginConfig where
  parseJSON = withObject "PluginConfig" $ \v -> do
    pluginsObj <- v .:? "plugins" .!= mempty
    plugins <- parsePluginMap pluginsObj
    wl <- v .:? "watchlist" .!= []
    pure $ PluginConfig plugins (Watchlist wl)

instance ToJSON PluginConfig where
  toJSON pc = object
    [ "plugins"   .= Map.map stripName (_pcPlugins pc)
    , "watchlist" .= _wlPaths (_pcWatchlist pc)
    ]
    where
      stripName p = object $
        maybe [] (\d -> ["description" .= d]) (_pluginDescription p)
        ++ (if null (_pluginPaths p) then [] else ["paths" .= _pluginPaths p])
        ++ (if null (_pluginDepends p) then [] else ["depends" .= _pluginDepends p])
        ++ maybe [] (\h -> ["post-install" .= h]) (_pluginPostInstall p)

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \v ->
    Profile <$> v .:  "name"
            <*> v .:? "plugins" .!= []

instance ToJSON Profile where
  toJSON p = object
    [ "name"    .= _profileName p
    , "plugins" .= _profilePlugins p
    ]

-- | ProfileConfig parses:
-- @
-- profiles:
--   work-mac:
--     plugins: [shell, git, neovim]
-- @
instance FromJSON ProfileConfig where
  parseJSON = withObject "ProfileConfig" $ \v -> do
    profilesObj <- v .:? "profiles" .!= mempty
    profiles <- parseProfileMap profilesObj
    pure $ ProfileConfig profiles

instance ToJSON ProfileConfig where
  toJSON pc = object
    [ "profiles" .= Map.map stripName (_prfProfiles pc)
    ]
    where
      stripName p = object
        [ "plugins" .= _profilePlugins p
        ]

instance FromJSON LocalState where
  parseJSON = withObject "LocalState" $ \v ->
    LocalState <$> v .:? "active-profile"
               <*> v .:? "installed-plugins" .!= []

instance ToJSON LocalState where
  toJSON ls = object
    [ "active-profile"    .= _lsActiveProfile ls
    , "installed-plugins" .= _lsInstalledPlugins ls
    ]

---------------
-- Utilities --
---------------

parsePluginMap :: Map.Map Text Value -> Parser (Map.Map PluginName Plugin)
parsePluginMap = Map.traverseWithKey $ \k val ->
  withObject "Plugin" (\v ->
    Plugin k
      <$> v .:? "description"
      <*> v .:? "paths" .!= []
      <*> v .:? "depends" .!= []
      <*> v .:? "post-install"
  ) val

parseProfileMap :: Map.Map Text Value -> Parser (Map.Map ProfileName Profile)
parseProfileMap = Map.traverseWithKey $ \k val ->
  withObject "Profile" (\v ->
    Profile k <$> v .:? "plugins" .!= []
  ) val
