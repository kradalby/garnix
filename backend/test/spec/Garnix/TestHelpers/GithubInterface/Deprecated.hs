module Garnix.TestHelpers.GithubInterface.Deprecated where

import Data.IORef.Lifted (IORef, atomicModifyIORef')
import Data.IntMap qualified as IntMap
import Garnix.Forge.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestInstances ()
import Garnix.Types
import GitHub.App.Auth qualified as GHA
import GitHub.Data.Id (mkId)
import System.Directory (doesFileExist)

defaultCommitHash :: CommitHash
defaultCommitHash = CommitHash "aaaa"

-- | deprecated: consider using mkFakeGithubInterface instead
testGithubInterface ::
  FilePath -> IORef (IntMap.IntMap [(Text, RunReportStatus, RawLogs)]) -> IO (Forge 'GitHub)
testGithubInterface tmp buildRef = do
  pure
    $ Forge
      { _forgeForgeKind = SGitHub,
        _forgeGetInstallation = \fid -> do
          appAuth <- githubAppConfigAuth <$> githubAppConfig
          iAuth <- liftIO $ GHA.mkInstallationAuth appAuth (mkId Proxy (fromInteger (getForgeInstallationId fid)))
          pure $ GithubAuth iAuth (ForgeToken "test-token"),
        _forgeGetInstallations = const $ pure [],
        _forgeGetAppInstallationId = \_ _ -> pure $ Just (ForgeInstallationId 1),
        _forgeGetAccessToken = const $ pure (ForgeToken "test-token"),
        _forgeGetDefaultBranch = \_ _ _ -> pure (Just $ Branch "main"),
        _forgeGetHeadCommit = \_ _ _ _ -> pure defaultCommitHash,
        _forgeGetRemote = \_ _ _ -> do
          pure $ RemoteUrl ("file:///" <> cs tmp <> "/.git"),
        _forgeDoesRepoFileExist = \_ _ _ path -> liftIO $ do
          doesFileExist (tmp </> path) >>= \case
            True -> pure FileExists
            False -> pure FileDoesntExist,
        _forgeNewBuildReport = \_ runReport -> do
          let next x = case IntMap.lookupMax x of
                Nothing -> 0
                Just (n, _) -> succ n
          int <-
            atomicModifyIORef'
              buildRef
              (\x -> (IntMap.insert (next x) [(runReport ^. name, runReport ^. status, RawLogs "")] x, next x))
          pure $ ForgeRunId $ fromIntegral int,
        _forgeUpdateBuildReport = \(ForgeRunId runId) runReport _ -> do
          let logs = _ghRunReportLogs runReport
          atomicModifyIORef' buildRef (\x -> (IntMap.insertWith (++) (fromIntegral runId) [(runReport ^. name, runReport ^. status, logs)] x, ())),
        _forgeGetRepoCollaborators = \_ _ _ -> pure $ Collaborators [],
        _forgeGetRepoPublicity = \_ _ _ -> return $ RepoIsPublic True,
        _forgeGetInstalledOrgs = \_ -> pure [],
        _forgeGetReposAccessibleTo = \_ _ -> pure [],
        _forgeOpenPullRequest = \_ _ _ -> pure $ PullRequestResult ""
      }
