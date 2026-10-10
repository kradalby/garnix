module Garnix.Sandbox (inNixSandbox, SandboxAccessType (..)) where

import Control.Concurrent.Async.Lifted (wait, withAsyncBound)
import Cradle
import Cradle.ProcessConfiguration
import Data.List
import Garnix.Monad hiding (workingDir)
import Garnix.NixConfig (getNetRcFileSetting)
import Garnix.Prelude
import Garnix.Types (NetRcFile (..))
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getEnv)

-- | Run a command in a sandbox appropriate for nix commands.
--
-- bwrap's @--die-with-parent@ fires when the OS thread that forked it exits, not
-- when this process does (prctl(2), PR_SET_PDEATHSIG), and the threaded RTS
-- retires surplus worker threads whenever it likes. Forking from a bound thread
-- that lives until the sandbox exits keeps the RTS from SIGKILLing it mid-run.
inNixSandbox :: (Output o) => [(FilePath, SandboxAccessType)] -> Maybe FilePath -> M ProcessConfiguration -> M o
inNixSandbox extraSandboxPaths xdgCacheHome procConfigM = do
  procConfig <- procConfigM
  path <- liftIO $ getEnv "PATH"
  let env = fromMaybe identity (environmentModification procConfig) [("PATH", path)]
  netrcFile <- do
    userNixConfig <- view #userNixConfig
    pure $ case getNetRcFileSetting userNixConfig of
      (Just (NetRcFile netrcFile)) -> [(netrcFile, ReadOnly)]
      Nothing -> []
  dir <- case workingDir procConfig of
    Nothing -> liftIO getCurrentDirectory
    Just d -> pure d
  args <- argsForNixSandbox xdgCacheHome dir (extraSandboxPaths ++ netrcFile) env
  let oldCommand = executable procConfig : arguments procConfig
      sandboxed = procConfig {executable = "bwrap", arguments = args <> oldCommand}
  withAsyncBound (run sandboxed) wait

data SandboxAccessType = TryReadOnly | ReadOnly | ReadWrite | LockFile
  deriving (Eq)

argsForNixSandbox :: (MonadIO m) => Maybe FilePath -> FilePath -> [(FilePath, SandboxAccessType)] -> [(String, String)] -> m [String]
argsForNixSandbox xdgCacheHome workingDir extraPaths env = do
  let extraPathOptions = flip concatMap extraPaths $ \case
        (path, TryReadOnly) -> ["--ro-bind-try", path, path]
        (path, ReadOnly) -> ["--ro-bind", path, path]
        (path, ReadWrite) -> ["--bind", path, path]
        (path, LockFile) -> ["--bind", path, path, "--lock-file", path]
  -- We use `-try` versions often since many of these dirs don't always exist.
  let baseArgs =
        ["--proc", "/proc"]
          <> ["--dev", "/dev"]
          <> ["--tmpfs", "/tmp"]
          <> ["--die-with-parent"]
          <> ["--unshare-pid"]
          <> ["--clearenv"]
          <> ["--ro-bind-try", "/bin/sh", "/bin/sh"]
  -- necessary for running tests on some non nixos systems
  let nonNixosSharedLibArgs =
        ["--ro-bind-try", "/lib", "/lib"]
          <> ["--ro-bind-try", "/lib64", "/lib64"]
  let nixArgs =
        ["--ro-bind-try", "/nix/", "/nix/"]
          <> ["--bind-try", "/nix/var/log/", "/nix/var/log/"]
          <> ["--ro-bind-try", "/etc/nix/machines", "/etc/nix/machines"]
          <> ["--ro-bind-try", "/etc/static/nix/machines", "/etc/static/nix/machines"]
          <> ["--ro-bind-try", "/etc/nix/nix.conf", "/etc/nix/nix.conf"]
          <> ["--ro-bind-try", "/etc/static/nix/nix.conf", "/etc/static/nix/nix.conf"]
  let networkingArgs =
        ["--ro-bind-try", "/etc/localtime", "/etc/localtime"]
          <> ["--ro-bind-try", "/etc/ssl", "/etc/ssl"]
          <> ["--ro-bind-try", "/etc/static/ssl", "/etc/static/ssl"]
          <> ["--ro-bind-try", "/etc/zoneinfo", "/etc/zoneinfo"]
          <> ["--ro-bind-try", "/etc/static/zoneinfo", "/etc/static/zoneinfo"]
          <> ["--ro-bind-try", "/etc/resolv.conf", "/etc/resolv.conf"]
          <> ["--ro-bind-try", "/etc/nsswitch.conf", "/etc/nsswitch.conf"]
          <> ["--ro-bind-try", "/etc/hostname", "/etc/hostname"]
          <> ["--ro-bind-try", "/etc/static/hostname", "/etc/static/hostname"]
  let systemProgramArgs = ["--ro-bind-try", "/run/current-system/sw", "/run/current-system/sw"]
  let sandboxHome = "/home/nix-runner"
  for_ xdgCacheHome $ \cache ->
    liftIO $ createDirectoryIfMissing True (cache </> "nix/gitv3")
  let homeArgs =
        ["--tmpfs", sandboxHome, "--setenv", "HOME", sandboxHome]
          <> ["--setenv", "XDG_CACHE_HOME", sandboxHome </> ".cache"]
          <> maybe
            ["--tmpfs", sandboxHome </> ".cache", "--tmpfs", sandboxHome </> ".cache/nix/gitv3"]
            (\dir -> ["--bind", dir, sandboxHome </> ".cache"])
            xdgCacheHome
  nixConfigForTests <- do
    hostHome <- liftIO $ getEnv "HOME"
    let nixConfigPath home = home </> ".config/nix/nix.conf"
    pure ["--ro-bind-try", nixConfigPath hostHome, nixConfigPath sandboxHome]
  let workingDirArgs = ["--bind", workingDir, workingDir]
  let setEnvs = concatMap (\(name, value) -> ["--setenv", name, value]) env
  pure
    $ baseArgs
    <> nonNixosSharedLibArgs
    <> nixArgs
    <> networkingArgs
    <> systemProgramArgs
    <> homeArgs
    <> nixConfigForTests
    <> workingDirArgs
    <> extraPathOptions
    <> setEnvs
