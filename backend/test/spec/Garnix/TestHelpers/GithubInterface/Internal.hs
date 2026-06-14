module Garnix.TestHelpers.GithubInterface.Internal where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Cradle qualified
import Data.Map
import Data.Maybe (fromJust)
import Garnix.Forge.Types
import Garnix.GithubInterface (githubRepoInfo)
import Garnix.GithubInterface.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers.Common (commitAll)
import Garnix.TestInstances ()
import Garnix.Types
import GitHub.App.Auth qualified as GHA
import GitHub.Data.Id (mkId)
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit (assertFailure)

data TestRepo = TestRepo
  { publicity :: RepoPublicity,
    collaborators :: [ForgeLogin],
    localPath :: Maybe FilePath,
    defaultBranch :: Maybe Branch,
    pullRequestBranch :: Maybe Branch
  }
  deriving stock (Generic)

newtype RepoCollection = RepoCollection (TVar (Map (RepoOwner, RepoName) TestRepo))

newRepoCollection :: M RepoCollection
newRepoCollection = liftIO $ RepoCollection <$> newTVarIO mempty

lookupRepoImpl :: RepoCollection -> RepoOwner -> RepoName -> M (Maybe TestRepo)
lookupRepoImpl (RepoCollection rc) owner name = do
  repos <- liftIO $ readTVarIO rc
  pure $ repos !? (owner, name)

updateRepo :: RepoCollection -> RepoOwner -> RepoName -> (TestRepo -> TestRepo) -> M ()
updateRepo (RepoCollection rc) owner name modify =
  liftIO
    $ atomically
    $ modifyTVar rc
    $ Data.Map.alter mergeTestRepos (owner, name)
  where
    mergeTestRepos :: Maybe TestRepo -> Maybe TestRepo
    mergeTestRepos = \case
      Nothing -> Just $ modify $ TestRepo (RepoIsPublic True) [] Nothing Nothing Nothing
      Just repo -> Just $ modify repo

setRepoImpl :: RepoCollection -> RepoOwner -> RepoName -> (TestRepo -> TestRepo) -> M ()
setRepoImpl repoCollection owner name modify = updateRepo repoCollection owner name $ const $ modify $ TestRepo (RepoIsPublic True) [] Nothing Nothing Nothing

withLocalRepoImpl :: RepoCollection -> RepoOwner -> RepoName -> CommitInfo -> (FilePath -> M ()) -> (CommitInfo -> M a) -> M a
withLocalRepoImpl rc owner name commitInfo setup action = do
  withSystemTempDirectory "garnix-test" $ \mockGithubRepo -> do
    setup mockGithubRepo
    let defBranch = fromJust $ commitInfo ^. branch
    Cradle.run_
      $ Cradle.cmd "git"
      & Cradle.setWorkingDir mockGithubRepo
      & Cradle.addArgs ["init", "." :: String]
      & Cradle.silenceStdout
      & Cradle.silenceStderr
    Cradle.run_
      $ Cradle.cmd "git"
      & Cradle.setWorkingDir mockGithubRepo
      & Cradle.addArgs ["checkout", "-b", getBranch defBranch]
      & Cradle.silenceStderr
    commit' <- commitAll mockGithubRepo
    updateRepo
      rc
      owner
      name
      ( (#localPath ?~ mockGithubRepo)
          . (#defaultBranch ?~ defBranch)
      )
    action (commitInfo & (commit .~ commit'))

data ReportCollection = ReportCollection
  { reports :: TVar (Map ForgeRunId [(RepoInfo, GhRunReport)]),
    nextGhRunId :: TVar ForgeRunId
  }

newReportCollection :: M ReportCollection
newReportCollection = liftIO $ ReportCollection <$> newTVarIO mempty <*> newTVarIO 0

appendNewReport :: ReportCollection -> RepoInfo -> GhRunReport -> M ForgeRunId
appendNewReport ReportCollection {..} repoInfo runReport = liftIO $ do
  id <- atomically $ do
    id <- readTVar nextGhRunId
    writeTVar nextGhRunId (id + 1)
    pure id

  atomically
    $ modifyTVar reports
    $ Data.Map.insert id [(repoInfo, runReport)]

  pure id

updateReport :: ReportCollection -> ForgeRunId -> GhRunReport -> RepoInfo -> M ()
updateReport ReportCollection {..} ghRunId runReport repoInfo =
  liftIO
    $ atomically
    $ modifyTVar reports
    $ Data.Map.insertWith (\new old -> old <> new) ghRunId [(repoInfo, runReport)]

getReportsImpl :: ReportCollection -> M [[(RepoInfo, GhRunReport)]]
getReportsImpl ReportCollection {..} =
  liftIO $ Data.Map.elems <$> readTVarIO reports

newtype OrgMembersCollection = OrgMembersCollection (TVar [UserOrgMembership])

newOrgMembersCollection :: M OrgMembersCollection
newOrgMembersCollection = liftIO $ OrgMembersCollection <$> newTVarIO []

addOrgMembersImpl :: OrgMembersCollection -> [UserOrgMembership] -> M ()
addOrgMembersImpl (OrgMembersCollection oc) toAdd =
  liftIO
    $ atomically
    $ modifyTVar
      oc
      (<> toAdd)

getOrgMembers :: OrgMembersCollection -> M [UserOrgMembership]
getOrgMembers (OrgMembersCollection oc) = liftIO $ readTVarIO oc

data GithubFakeState = GithubFakeState
  { repoCollection :: RepoCollection,
    reportCollection :: ReportCollection,
    orgMembersCollection :: OrgMembersCollection
  }

-- | A placeholder GitHub 'RepoInfo' for tests. Its forge delegates to whatever is
-- in 'Env.forges' (the injected fake), so reporting\/remote calls hit the fake; the
-- installation auth is left unset (forced only if a test path needs it).
fakeRepoInfo :: RepoOwner -> RepoName -> RepoInfo
fakeRepoInfo = githubRepoInfo (error "fakeRepoInfo: installation auth not set") (ForgeToken "")

-- | A forge registry that serves a single fake GitHub forge (and errors for other
-- forges). Used to populate 'Env.forges' in tests.
githubForgeRegistry :: Forge 'GitHub -> ForgeKind -> SomeForge
githubForgeRegistry f = \case
  GitHub -> SomeForge f
  k -> error $ "test forge registry: no fake forge for " <> show k

-- | Reconstruct a 'RepoInfo' from a GitHub 'ForgeRepo', for storing alongside
-- reports (tests inspect the owner\/name via 'RepoInfo' lenses).
fakeForgeRepoToRepoInfo :: ForgeRepo 'GitHub -> RepoInfo
fakeForgeRepoToRepoInfo fr = RepoInfo (SomeForgeRepo fr) (_forgeRepoOwner fr) (_forgeRepoName fr)

mkFakeGithubInterface :: M (GithubFakeState, Forge 'GitHub)
mkFakeGithubInterface = do
  repoCollection <- newRepoCollection
  reportCollection <- newReportCollection
  orgMembersCollection <- newOrgMembersCollection
  let notImplemented methodName = error $ methodName <> " not implemented in mkFakeGithubInterface"
  pure
    ( GithubFakeState
        { repoCollection = repoCollection,
          reportCollection = reportCollection,
          orgMembersCollection = orgMembersCollection
        },
      Forge
        { _forgeForgeKind = SGitHub,
          _forgeGetAccessToken = \_ -> pure $ notImplemented "_forgeGetAccessToken",
          _forgeGetDefaultBranch = \_ repoOwner repoName -> do
            repo <- lookupRepoImpl repoCollection repoOwner repoName
            pure $ repo >>= \r -> r ^. #defaultBranch,
          _forgeGetHeadCommit = \_ repoOwner repoName branch -> do
            repo <- lookupRepoImpl repoCollection repoOwner repoName
            case repo of
              Nothing ->
                throw
                  $ OtherError
                  $ "fakeGithubInterrface/getHeadCommit: could not find repository "
                  <> getForgeLogin (getRepoOwner repoOwner)
                  <> "/"
                  <> getRepoName repoName
              Just repo -> do
                when (repo ^. #defaultBranch /= Just branch)
                  $ throw
                  $ OtherError
                  $ "fakeGithubInterface/getHeadCommit: can only get head commit for default branch ("
                  <> show (repo ^. #defaultBranch)
                  <> ") but got "
                  <> show branch

                case repo ^. #localPath of
                  Nothing -> throw $ OtherError "fakeGithubInterface/getHeadCommit: can only get head commit for local repos (localPath must be set up)"
                  Just path -> do
                    commitHasFlakeNix <-
                      Cradle.run
                        $ Cradle.cmd "git"
                        & Cradle.setWorkingDir path
                        & Cradle.addArgs ["rev-parse", getBranch branch]
                        & Cradle.silenceStderr
                    case commitHasFlakeNix of
                      (Cradle.ExitFailure _, _) -> liftIO $ assertFailure "could not find git branch"
                      (Cradle.ExitSuccess, Cradle.StdoutTrimmed stdout) -> do
                        pure $ CommitHash $ cs stdout,
          _forgeNewBuildReport = \fr report -> appendNewReport reportCollection (fakeForgeRepoToRepoInfo fr) report,
          _forgeUpdateBuildReport = \runId report fr -> updateReport reportCollection runId report (fakeForgeRepoToRepoInfo fr),
          _forgeDoesRepoFileExist = \fr _commit _mFork relativePath -> do
            let owner = _forgeRepoOwner fr
                name = _forgeRepoName fr
            repo <- lookupRepoImpl repoCollection owner name
            case repo >>= \r -> r ^. #localPath of
              Nothing ->
                liftIO
                  $ assertFailure
                  $ cs
                  $ "Trying to access mocked repository '"
                  <> getForgeLogin (getRepoOwner owner)
                  <> "/"
                  <> getRepoName name
                  <> "' at path '"
                  <> cs relativePath
                  <> "' without setting it."
              Just basePath ->
                liftIO (doesFileExist (basePath </> relativePath)) >>= \case
                  True -> pure FileExists
                  False -> pure FileDoesntExist,
          _forgeGetInstalledOrgs = \_tok -> getOrgMembers orgMembersCollection,
          _forgeGetRemote = \fr _commit _mFork -> do
            let owner = _forgeRepoOwner fr
                name = _forgeRepoName fr
            repo <- lookupRepoImpl repoCollection owner name
            case repo >>= \r -> r ^. #localPath of
              Nothing ->
                liftIO
                  $ assertFailure
                  $ cs
                  $ "Trying to access mocked repository remote for '"
                  <> getForgeLogin (getRepoOwner owner)
                  <> "/"
                  <> getRepoName name
              Just basePath -> pure $ RemoteUrl ("file:///" <> cs basePath <> "/.git"),
          _forgeGetInstallation = \fid -> do
            appAuth <- githubAppConfigAuth <$> githubAppConfig
            iAuth <- liftIO $ GHA.mkInstallationAuth appAuth (mkId Proxy (fromInteger (getForgeInstallationId fid)))
            pure $ GithubAuth iAuth (ForgeToken "fake-token"),
          _forgeGetInstallations = const $ pure [],
          _forgeGetAppInstallationId = \_ _ -> pure $ Just (ForgeInstallationId 1),
          _forgeGetRepoPublicity = \_ owner name -> do
            repo <- lookupRepoImpl repoCollection owner name
            case repo of
              Just repo -> pure $ repo ^. #publicity
              Nothing -> throw $ NoSuchRepo {_owner = owner, _name = name},
          _forgeGetRepoCollaborators = \_iAuth owner repo -> do
            repo <- lookupRepoImpl repoCollection owner repo
            case repo of
              Nothing -> pure RepoNotFound
              Just r -> do
                -- Github returns the owner in the collaborators list
                pure $ Collaborators (getRepoOwner owner : r ^. #collaborators),
          _forgeGetReposAccessibleTo = \_ _ -> pure [],
          _forgeOpenPullRequest = \owner@(RepoOwner (ForgeLogin o)) repo@(RepoName r) pr -> do
            updateRepo repoCollection owner repo (#pullRequestBranch ?~ (pr ^. headBranch))

            repo <- lookupRepoImpl repoCollection owner repo
            case repo of
              Nothing -> throw NotFound
              Just _ ->
                pure $ PullRequestResult $ cs o <> "/" <> cs r <> "/pulls/1"
        }
    )
