{-# LANGUAGE DuplicateRecordFields #-}

module Garnix.Orchestrator
  ( handlePullRequest,
    handleCommit,
    handleRerun,
    RerunEvent (..),
    ForgeEvent (..),
    dispatchForgeEvent,
    forgeReporter,
  )
where

import Garnix.Async (Promise)
import Garnix.Build (buildFlake, rerunBuild)
import Garnix.Build.Checkout qualified as Build.Checkout
import Garnix.Build.Helpers (withInternalCacheToken)
import Garnix.DB qualified as DB
import Garnix.Hosting.Deploy (rolloutNewServerVersion)
import Garnix.Monad
import Garnix.Monad.Async (emptyPromise, resolve, spawn)
import Garnix.Prelude
import Garnix.GithubInterface (githubRepoInfo)
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.Reporters.OpenSearchReporter (openSearchReporter)
import Garnix.Types hiding (ghRunId)
import Garnix.Types qualified as Types
import GitHub.App.Auth qualified as GH

data RerunEvent = RerunEvent
  { reqUser :: ForgeLogin,
    -- | The build to rerun, recovered from the check run's @external_id@.
    originalBuildId :: BuildId,
    installAuth :: GH.InstallationAuth,
    token :: ForgeToken,
    repoIsPublic :: RepoPublicity
  }
  deriving stock (Generic)

-- | A forge-neutral, normalized webhook event. Per-forge webhook parsers (GitHub,
-- Gitea, GitLab) translate their wire formats into this; 'dispatchForgeEvent' is
-- the single, shared entry point into the build pipeline.
data ForgeEvent
  = -- | A commit/branch build was requested (GitHub check-suite or push). The
    -- 'Bool' is whether a duplicate run is allowed (i.e. it's a rerequest).
    CommitBuild Bool CommitInfo
  | -- | A pull/merge request build was requested.
    PullRequestBuild CommitInfo PullRequestId
  | -- | A provider-native rerun of a single build was requested.
    Rerun RerunEvent

-- | The reporter for a commit's forge: OpenSearch plus the forge's own build
-- reporting (which dispatches through the repo's forge bundle).
forgeReporter :: CommitInfo -> Reporter
forgeReporter commitInfo =
  openSearchReporter <> mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)

-- | Dispatch a normalized 'ForgeEvent' into the (forge-agnostic) build pipeline.
dispatchForgeEvent :: (HasCallStack) => ForgeEvent -> M (Promise ())
dispatchForgeEvent = \case
  CommitBuild allowDuplicateRun commitInfo ->
    handleCommit (forgeReporter commitInfo) allowDuplicateRun commitInfo
  PullRequestBuild commitInfo prId ->
    handlePullRequest (forgeReporter commitInfo) commitInfo prId
  Rerun ev -> handleRerun ev >> emptyPromise

handlePullRequest :: (HasCallStack) => Reporter -> CommitInfo -> PullRequestId -> M (Promise ())
handlePullRequest reporter commitInfo prId = do
  assertIsAllowedToBuild (commitInfo ^. repoInfo . ghRepoOwner) (commitInfo ^. repoInfo . ghRepoName)

  withSpan commitInfo $ spawn $ do
    -- We assume the build is already running if it's NOT a fork.
    when (isJust $ commitInfo ^. prFromFork) $ do
      buildFlake reporter commitInfo >>= resolve

    -- If it's not a fork, we're already attempting to deploy in `buildFlake`.
    when (isNothing $ commitInfo ^. prFromFork) $ do
      deployPrServers prId
  where
    deployPrServers :: PullRequestId -> M ()
    deployPrServers prId = do
      Build.Checkout.withCheckout commitInfo $ withSpan prId $ do
        withInternalCacheToken (commitInfo ^. Types.reqUser) $ do
          void (rolloutNewServerVersion reporter commitInfo $ GhPrDeployment prId)

handleCommit :: (HasCallStack) => Reporter -> Bool -> CommitInfo -> M (Promise ())
handleCommit reporter allowDuplicateRun commitInfo = do
  withSpan commitInfo $ do
    assertIsAllowedToBuild (commitInfo ^. repoInfo . ghRepoOwner) (commitInfo ^. repoInfo . ghRepoName)
    pushResult <- case commitInfo ^. branch of
      Nothing -> do
        log Informational "handleCommit: CommitInfo is missing branch. Not registering push"
        pure Nothing
      Just branch -> do
        Just
          <$> DB.registerPush
            (commitInfo ^. repoInfo . ghRepoOwner)
            (commitInfo ^. repoInfo . ghRepoName)
            (commitInfo ^. commit)
            branch
    case (allowDuplicateRun, pushResult) of
      (False, Just DB.AlreadyPushed) -> do
        log Informational "handleCommit: This repoOwner, repoName, commit, branch combination has already been pushed before. Skipping build"
        emptyPromise
      (False, Nothing) -> do
        log Informational "handleCommit: CommitInfo is missing branch, but allowDuplicateRun is set. Skipping build"
        emptyPromise
      (False, Just DB.NewPush) -> do
        buildFlake reporter commitInfo <?> "Build flake"
      (True, _) -> do
        buildFlake reporter commitInfo <?> "Build flake"

handleRerun :: (HasCallStack) => RerunEvent -> M ()
handleRerun ev = do
  hostname <- view #hostname
  build' <- DB.makeNewBuildFromBuildId (ev ^. #reqUser) (ev ^. #originalBuildId) hostname
  withSpan (build' ^. id) $ do
    let commitInfo =
          CommitInfo
            { _commitInfoReqUser = ev ^. #reqUser,
              _commitInfoRepoPublicity = ev ^. #repoIsPublic,
              _commitInfoRepoInfo = githubRepoInfo (ev ^. #installAuth) (ev ^. #token) (build' ^. repoUser) (build' ^. repoName),
              _commitInfoBranch = build' ^. branch,
              _commitInfoPrFromFork = build' ^. prFromFork,
              _commitInfoCommit = build' ^. gitCommit
            }
    let reporter = openSearchReporter <> mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit)
    assertIsAllowedToBuild (build' ^. repoUser) (build' ^. repoName)
    withSpan commitInfo $ rerunBuild reporter build' commitInfo

assertIsAllowedToBuild :: RepoOwner -> RepoName -> M ()
assertIsAllowedToBuild owner repo = do
  isDenied <- DB.isDenylisted owner repo
  when isDenied $ do
    throw IsDeniedAccess
