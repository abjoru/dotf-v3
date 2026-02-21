module Dotf.Packages (
  Distro(..),
  detectDistro,
  collectPackages,
  collectCaskPackages,
  listInstalledPackages,
  filterUninstalled,
  installPackagesCli,
) where

import qualified Data.ByteString.Lazy     as BL
import           Data.List                (nub)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO             as TIO
import           Dotf.Types
import           System.Directory         (doesFileExist)
import           System.Exit              (ExitCode (..))
import           System.Info              (os)
import qualified System.Process.Typed     as PT

data Distro = Arch | Osx | UnsupportedDistro
  deriving (Show, Eq)

-- | Detect current OS distro.
detectDistro :: IO Distro
detectDistro
  | os == "darwin" = pure Osx
  | otherwise = do
      exists <- doesFileExist "/etc/os-release"
      if not exists then pure UnsupportedDistro
      else do
        content <- TIO.readFile "/etc/os-release"
        let idLine = filter ("ID=" `T.isPrefixOf`) (T.lines content)
        case idLine of
          (l:_) | "arch"    `T.isInfixOf` l -> pure Arch
                | "cachyos" `T.isInfixOf` l -> pure Arch
          _ -> pure UnsupportedDistro

-- | Collect all packages for the given distro from a list of plugins.
collectPackages :: Distro -> [Plugin] -> [Text]
collectPackages Arch    ps = nub $ concatMap _pluginArch ps
collectPackages Osx     ps = nub $ concatMap (\p -> _pluginOsx p ++ _pluginCask p) ps
collectPackages UnsupportedDistro _ = []

-- | Extract cask packages (macOS only). Used to separate brew vs cask installs.
collectCaskPackages :: [Plugin] -> [Text]
collectCaskPackages = nub . concatMap _pluginCask

-- | List installed packages for the given distro.
listInstalledPackages :: Distro -> IO [Text]
listInstalledPackages Arch = do
  (exitCode, out, _) <- PT.readProcess (PT.proc "pacman" ["-Qq"])
  case exitCode of
    ExitSuccess   -> pure $ T.lines (T.strip $ decodeLazy out)
    ExitFailure _ -> pure []
listInstalledPackages Osx = do
  (exitCode, out, _) <- PT.readProcess (PT.proc "brew" ["list", "--formula", "-1"])
  case exitCode of
    ExitSuccess   -> do
      let formulas = T.lines (T.strip $ decodeLazy out)
      (exitCode2, out2, _) <- PT.readProcess (PT.proc "brew" ["list", "--cask", "-1"])
      case exitCode2 of
        ExitSuccess   -> pure $ formulas ++ T.lines (T.strip $ decodeLazy out2)
        ExitFailure _ -> pure formulas
    ExitFailure _ -> pure []
listInstalledPackages UnsupportedDistro = pure []

-- | Filter packages that are not in the installed list.
filterUninstalled :: [Text] -> [Text] -> [Text]
filterUninstalled installed =
  let installedSet = Set.fromList installed
  in filter (`Set.notMember` installedSet)

-- | Install packages using the appropriate package manager.
-- First argument: regular packages; second: cask packages (macOS only).
installPackagesCli :: Distro -> [Text] -> [Text] -> IO ()
installPackagesCli Arch regular _ = do
  _ <- PT.runProcess (PT.proc "paru" ("-S" : "--needed" : map T.unpack regular))
  pure ()
installPackagesCli Osx regular casks = do
  if null regular then pure ()
  else do
    _ <- PT.runProcess (PT.proc "brew" ("install" : map T.unpack regular))
    pure ()
  if null casks then pure ()
  else do
    _ <- PT.runProcess (PT.proc "brew" ("install" : "--cask" : map T.unpack casks))
    pure ()
installPackagesCli UnsupportedDistro _ _ = pure ()

-----------
-- Utils --
-----------

decodeLazy :: BL.ByteString -> Text
decodeLazy = decodeUtf8With lenientDecode . BL.toStrict
