module Garnix.GithubInterface
  ( githubForge,
    githubRepoInfo,
    fromRunReport,
    -- exported for testing
    _retryWhen,
    _retryGithubRequest,
  )
where

import Control.Retry (RetryPolicyM, fullJitterBackoff, limitRetries, retryOnError, retrying)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Array, _Integer, _Integral, _String)
import Data.Row (Rec, type (.==))
import Data.Tagged (Tagged (..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Vector qualified as Vector
import Garnix.BuildLogs
import Garnix.Duration
import Garnix.Forge.Types
import Garnix.GithubInterface.Types
import Garnix.Monad hiding
  ( getInstallations,
    getInstalledOrgs,
    getReposInInstallationAccessibleTo,
  )
import Garnix.Prelude
import Garnix.Types hiding (statusCode)
import GitHub qualified as GH
import GitHub.App.Auth qualified as GHA
import GitHub.App.Request qualified as GHA
import GitHub.Data.Id
import GitHub.Data.Installations qualified as GHA
import GitHub.Data.Name qualified as GH
import Network.HTTP.Client qualified
import Network.HTTP.Client.Internal qualified
import Network.HTTP.Conduit (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Types (status500, statusCode)
import Network.Wreq qualified as Wreq
import Web.JWT qualified as JWT

getInstallations :: ForgeToken -> M [GH.Id GHA.Installation]
getInstallations (ForgeToken userToken) = do
  let auth = GH.OAuth (cs userToken)
      request =
        GH.query
          [ "user",
            "installations"
          ]
          [("per_page", Just "100")]
  result <- executeRequest auth request
  case result of
    Left githubError
      | isTimeout githubError -> throw GithubRequestTimeout
      | otherwise -> do
          log Error
            $ "getInstallations failed. Error was:"
            <> show githubError
          throw $ OtherError $ show githubError
    Right v ->
      pure
        $ (v :: Aeson.Value)
        ^.. key "installations"
        . _Array
        . each
        . key "id"
        . _Integral
        . to (GH.mkId Proxy)

getHeadCommitForBranch :: ForgeToken -> RepoOwner -> RepoName -> Branch -> M CommitHash
getHeadCommitForBranch (ForgeToken token) owner repo branch = do
  let auth = GH.OAuth (cs token)
      request =
        GH.query
          [ "repos",
            cs . getForgeLogin . getRepoOwner $ owner,
            cs . getRepoName $ repo,
            "branches",
            cs branch
          ]
          []
  executeRequest @Aeson.Value auth request
    >>= handleGithubRequestErrors "getHeadCommit" owner repo
    >>= maybe throwBadFormat pure
    . (^? key "commit" . key "sha" . _String . to CommitHash)
  where
    throwBadFormat :: M a
    throwBadFormat = do
      let context = show owner <> "/" <> show repo <> "/" <> show branch
      log Error $ "_githubInterfaceGetHeadCommit failed for '" <> context <> "': Could not find 'commit.sha'."
      throw . OtherError $ "Could not get the HEAD commit for " <> context

openGithubPullRequestInternal :: RepoOwner -> RepoName -> PullRequest -> M PullRequestResult
openGithubPullRequestInternal ghOwner@(RepoOwner (ForgeLogin owner)) ghRepo@(RepoName repo) pr = do
  installationId <- getGarnixInstallationId ghOwner ghRepo
  iAuth <- case installationId of
    Nothing -> throw $ GarnixAppUnauthorized ghOwner ghRepo
    Just id -> getInstallation (Id $ fromInteger id)
  let createPullRequest =
        GH.CreatePullRequest
          { GH.createPullRequestTitle = pr ^. title,
            GH.createPullRequestBody = pr ^. body,
            GH.createPullRequestHead = getBranch $ pr ^. headBranch,
            GH.createPullRequestBase = getBranch $ pr ^. baseBranch
          }
      request = GH.createPullRequestR (GH.mkName Proxy owner) (GH.mkName Proxy repo) createPullRequest
  result <- executeAppRequest iAuth request
  case result of
    Left githubError
      | isTimeout githubError -> throw GithubRequestTimeout
      | otherwise -> do
          log Error
            $ "openGithubPullRequest failed for installation '"
            <> owner
            <> "/"
            <> repo
            <> "'. Error was: "
            <> show githubError
          throw $ OtherError $ show githubError
    Right (GH.PullRequest {GH.pullRequestHtmlUrl = GH.URL url}) ->
      pure $ PullRequestResult {_pullRequestResultUrl = url}

getReposInInstallationAccessibleTo :: GH.Id GHA.Installation -> ForgeToken -> M [Text]
getReposInInstallationAccessibleTo installationId (ForgeToken userToken) = do
  let auth = GH.OAuth (cs userToken)
      request =
        GH.query
          [ "user",
            "installations",
            show $ GH.untagId installationId,
            "repositories"
          ]
          [("per_page", Just "100")]
  result <- executePaginatedRequest auth request
  case result of
    Left githubError
      | isTimeout githubError -> throw GithubRequestTimeout
      | otherwise -> do
          log Error
            $ "getReposInInstallationAccessibleTo failed for installation "
            <> show installationId
            <> ". Error was: "
            <> show githubError
          throw $ OtherError $ show githubError
    Right vs -> do
      repositories <-
        forM vs
          $ \( v ::
                 Rec
                   ( "repositories"
                       .== [ Rec
                               ("full_name" .== Text)
                           ]
                   )
               ) -> do
              pure $ map (^. #full_name) $ v ^. #repositories
      pure $ concat repositories

getInstalledOrgs :: ForgeToken -> M [UserOrgMembership]
getInstalledOrgs (ForgeToken tok) = do
  -- This endpoint lists org memberships for which these conditions are met:
  --
  -- 1. The user is a member of the org,
  -- 2. the garnix app is installed in one of the repos of the org.
  --
  -- Additionally, our github app has to have the permission `"members": "read"`.
  -- Without that permission the endpoint returns a 200 response, but the
  -- result is always the empty list.
  --
  -- I think this is *not* documented on github, see
  -- https://docs.github.com/en/rest/orgs/members?apiVersion=2022-11-28#list-organization-memberships-for-the-authenticated-user
  let endpoint = "https://api.github.com/user/memberships/orgs?per_page=100&state=active"
  response <-
    retryWreq $ withWreqOptions $ \options ->
      Wreq.getWith
        ( options
            & Wreq.auth
            ?~ Wreq.oauth2Token (cs tok)
            & Wreq.checkResponse
            ?~ \_ _ -> pure ()
        )
        endpoint

  log Informational
    $ "getInstalledOrgs status: "
    <> cs (show $ response ^. Wreq.responseStatus . Wreq.statusCode)
    <> "("
    <> cs (response ^. Wreq.responseStatus . Wreq.statusMessage)
    <> ")"
    <> " response: "
    <> cs (response ^. Wreq.responseBody)

  when (response ^. Wreq.responseStatus . Wreq.statusCode >= 400)
    $ throw
    $ OtherError
    $ "getInstalledOrgs unexpected status code from github: "
    <> show (response ^. Wreq.responseStatus . Wreq.statusCode)

  memberships :: [UserOrgMembership] <-
    aesonDecode
      ("response from " <> cs endpoint)
      parseJSON
      (response ^. Wreq.responseBody . to cs)
  log Informational $ "getInstalledOrgs all org members: " <> show memberships
  when (length memberships > 99) $ do
    throw . OtherError $ "too many installations - pagination not implemented yet"
  pure memberships

-- * Making Github requests

createBuildReportGH :: (HasCallStack) => GHA.InstallationAuth -> RepoOwner -> RepoName -> GhRunReport -> M ForgeRunId
createBuildReportGH iAuth owner@(RepoOwner (ForgeLogin repoUser)) repo@(RepoName repoName) report = do
  run <- fromRunReport report
  res <-
    executeAppRequest @Aeson.Value iAuth
      $ GH.Command GH.Post ["repos", repoUser, repoName, "check-runs"] (Aeson.encode run)
  handleGithubRequestErrors "createBuildReportGH" owner repo res >>= \case
    v ->
      case v ^? key "id" . _Integer of
        Nothing -> throw $ FailedToParseCreateReportResult v
        Just v -> pure $ fromInteger v

updateBuildReportGH :: (HasCallStack) => ForgeRunId -> GhRunReport -> GHA.InstallationAuth -> RepoOwner -> RepoName -> M ()
updateBuildReportGH runId report iAuth owner@(RepoOwner (ForgeLogin repoUser)) repo@(RepoName repoName) = do
  run <- fromRunReport report
  res <-
    executeAppRequest @Aeson.Value iAuth
      $ GH.Command GH.Patch ["repos", repoUser, repoName, "check-runs", show runId] (Aeson.encode run)
  handleGithubRequestErrors "updateBuildReportGH" owner repo res $> ()

fromRunReport :: GhRunReport -> M GhRun
fromRunReport (GhRunReport name commit url status' title summary logs' externalId) = do
  fromRelativeUrl <- relativeUrlConverter
  pure
    $ GhRun
      { _ghRunName = name,
        _ghRunHeadSha = commit,
        _ghRunExternalId = externalId,
        _ghRunDetailsUrl = fromRelativeUrl <$> url,
        _ghRunStatus = case status' of
          RunReportStatusInProgress -> "in_progress"
          _ -> "completed",
        _ghRunOutput =
          Just
            $ RunOutput
              { _runOutputTitle = title,
                _runOutputSummary = summary,
                _runOutputText = processLogsForGithub logs'
              },
        _ghRunConclusion = case status' of
          RunReportStatusInProgress -> Nothing
          RunReportStatusSuccess -> Just "success"
          RunReportStatusFailure -> Just "failure"
          RunReportStatusTimeout -> Just "timed_out"
          RunReportStatusCancelled -> Just "cancelled"
      }

handleGithubRequestErrors :: (HasCallStack) => Text -> RepoOwner -> RepoName -> Either GH.Error a -> M a
handleGithubRequestErrors method owner name = \case
  Left githubError | hasStatus (== 404) githubError -> do
    log Informational $ "Request " <> method <> " failed with 404. Error: " <> show githubError
    throw $ NoSuchRepo {_owner = owner, _name = name}
  Left githubError | hasStatus (== 403) githubError -> do
    log Informational $ "Request " <> method <> " failed with 403. Error: " <> show githubError
    throw $ GarnixAppUnauthorized owner name
  Left githubError | isTimeout githubError -> do
    log Informational $ "Request " <> method <> " timed out"
    throw GithubRequestTimeout
  Left err -> do
    log Error $ method <> " failed for '" <> show owner <> "/" <> show name <> "'. Error: " <> show err
    throw $ OtherError $ show err
  Right res -> pure res

isTimeout :: GH.Error -> Bool
isTimeout (GH.HTTPError (HttpExceptionRequest _ ResponseTimeout)) = True
isTimeout (GH.HTTPError (HttpExceptionRequest _ ConnectionTimeout)) = True
isTimeout (GH.HTTPError (HttpExceptionRequest _ NoResponseDataReceived)) = True
isTimeout (GH.HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) =
  statusCode (responseStatus r) == 504
isTimeout _ = False

handleGithubWreqErrors :: (Show a) => Text -> Wreq.Response a -> M (Maybe a)
handleGithubWreqErrors method response = do
  let status = response ^. Wreq.responseStatus . Wreq.statusCode
  case status of
    403 -> log Notice (method <> ": Github responded with 403") $> Nothing
    404 -> log Notice (method <> ": Github responded with 404") $> Nothing
    st | st > 399 -> do
      log Critical
        $ method
        <> ": Github responded with"
        <> cs (show $ response ^. Wreq.responseStatus . Wreq.statusCode)
        <> "("
        <> cs (response ^. Wreq.responseStatus . Wreq.statusMessage)
        <> ")"
        <> " response: "
        <> cs (show $ response ^. Wreq.responseBody)
      throw $ OtherError $ "Unexpected Github response for " <> method <> ": " <> show st
    _ -> pure . Just $ response ^. Wreq.responseBody

executeRequest :: (FromJSON a, Show a) => GH.Auth -> GH.Request rw a -> M (Either GH.Error a)
executeRequest auth request = do
  mgr <- view #manager
  _retryGithubRequest $ liftIO $ GH.executeRequestWithMgr mgr auth request

executeAppRequest :: (FromJSON a, Show a) => GHA.InstallationAuth -> GH.Request rw a -> M (Either GH.Error a)
executeAppRequest iAuth request = do
  mgr <- view #manager
  _retryGithubRequest $ liftIO $ GHA.executeAppRequestWithMgr mgr iAuth request

executePaginatedRequest :: forall a rw. (FromJSON a) => GH.Auth -> GH.Request rw a -> M (Either GH.Error [a])
executePaginatedRequest auth request = runExceptT $ do
  firstRequest <- liftIO $ GH.makeHttpRequest (Just auth) request
  inner firstRequest
  where
    inner :: Network.HTTP.Client.Request -> ExceptT GH.Error M [a]
    inner rawRequest = do
      manager' <- view #manager
      response <-
        retryExceptT
          ( liftIO (Network.HTTP.Client.httpLbs rawRequest manager')
              `catch` \(e :: HttpException) -> throwError (GH.HTTPError e)
          )
      parsed <- unTagged @'GH.MtJSON $ GH.parseResponse rawRequest response
      rest <- case GH.getNextUrl response of
        Nothing -> pure []
        Just nextUrl -> do
          nextRequest <-
            either (throwError . GH.ParseError . cs) pure
              $ Network.HTTP.Client.Internal.setUriEither rawRequest nextUrl
          inner nextRequest
      pure $ parsed : rest

retryPolicy :: (MonadIO m) => RetryPolicyM m
retryPolicy =
  limitRetries 5 <> fullJitterBackoff (toMicroseconds (fromSeconds @Int 1))

_retryWhen :: (Show response) => (response -> Bool) -> M response -> M response
_retryWhen shouldRetry action =
  retrying retryPolicy shouldRetryInner $ \_status -> do
    action
  where
    shouldRetryInner _status response = do
      let retrying = shouldRetry response
      when retrying $ do
        withTextSpan ("github-api-retry", "true") $ do
          log Warning $ show response
      pure retrying

retryWreq :: (Show body) => M (Wreq.Response body) -> M (Wreq.Response body)
retryWreq = _retryWhen $ \response -> (response ^. Wreq.responseStatus) >= status500

_retryGithubRequest :: (Show a) => M (Either GH.Error a) -> M (Either GH.Error a)
_retryGithubRequest = _retryWhen shouldRetry

shouldRetry :: Either GH.Error a -> Bool
shouldRetry = \case
  Right _ -> False
  Left error -> case error of
    GH.ParseError _ -> True
    GH.JsonError _ -> True
    GH.UserError _ -> False
    GH.HTTPError error -> case error of
      InvalidUrlException _ _ -> False
      HttpExceptionRequest _ error -> case error of
        StatusCodeException response _ -> statusCode (responseStatus response) >= 500
        InvalidRequestHeader _ -> False
        InvalidDestinationHost _ -> False
        InvalidProxyEnvironmentVariable _ _ -> False
        InvalidProxySettings _ -> False
        _ -> True

retryExceptT :: (MonadIO m) => ExceptT GH.Error m a -> ExceptT GH.Error m a
retryExceptT action = retryOnError retryPolicy (\_status error -> pure $ hasStatus (>= 500) error) (const action)

hasStatus :: (Int -> Bool) -> GH.Error -> Bool
hasStatus check (GH.HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) =
  check (statusCode (responseStatus r))
hasStatus _ _ = False

-- * GitHub 'Forge' implementation
--
-- 'githubForge' is the kind-indexed 'Forge' for GitHub. It reuses the helpers
-- above; the only adaptation versus 'realGithubInterface' is that credentials are
-- passed as @'ForgeAuth' 'GitHub'@ (which bundles the installation auth and a
-- token) rather than as a bare @GHA.InstallationAuth@.

obtainGhToken :: (HasCallStack) => GHA.InstallationAuth -> M ForgeToken
obtainGhToken iAuth = do
  mgr <- view #manager
  _retryGithubRequest (liftIO (GHA.obtainAccessToken mgr iAuth))
    >>= handleGithubRequestErrors "getAccessToken" "garnix" "garnix-github-app"
    >>= \case
      (GH.OAuth v) -> pure $ ForgeToken $ cs v
      _ -> throw $ OtherError "getAccessToken: unexpected auth token type"

garnixInstallationIdGH :: (HasCallStack) => RepoOwner -> RepoName -> M (Maybe Integer)
garnixInstallationIdGH (RepoOwner (ForgeLogin owner)) (RepoName repoName) = do
  appAuth <- githubAppConfigAuth <$> githubAppConfig
  currentTime <- utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
  let expireTime = currentTime + (550 :: NominalDiffTime)
  let claims =
        JWT.JWTClaimsSet
          { JWT.iat = JWT.numericDate currentTime,
            JWT.exp = JWT.numericDate expireTime,
            JWT.iss = JWT.stringOrURI . show . GH.untagId . GHA.aaAppId $ appAuth,
            JWT.sub = Nothing,
            JWT.nbf = Nothing,
            JWT.aud = Nothing,
            JWT.jti = Nothing,
            JWT.unregisteredClaims = mempty
          }
  let jwt = JWT.encodeSigned (JWT.EncodeRSAPrivateKey $ GHA.aaPrivateKey appAuth) mempty claims
  let url = "https://api.github.com/repos" </> cs owner </> cs repoName </> "installation"
  response <-
    retryWreq $ withWreqOptions $ \options ->
      Wreq.getWith
        (options & Wreq.auth ?~ Wreq.oauth2Bearer (cs jwt) & Wreq.checkResponse ?~ \_ _ -> pure ())
        url
  handleGithubWreqErrors "garnixInstallationIdGH" response >>= \mResponse ->
    pure $ mResponse >>= (^? key "id" . _Integer)

getDefaultBranchGH :: (HasCallStack) => Maybe GHA.InstallationAuth -> RepoOwner -> RepoName -> M (Maybe Branch)
getDefaultBranchGH miAuth owner repo = case miAuth of
  Nothing -> do
    mgr <- view #manager
    _retryGithubRequest (liftIO (GH.executeRequestWithMgr' mgr (GH.repositoryR (coerce owner) (coerce repo))))
      >>= handleGithubRequestErrors "getDefaultBranch" owner repo
      >>= \branch -> pure $ Branch <$> GH.repoDefaultBranch branch
  Just iAuth ->
    executeAppRequest iAuth (GH.repositoryR (coerce owner) (coerce repo))
      >>= handleGithubRequestErrors "getDefaultBranch" owner repo
      >>= \branch -> pure $ Branch <$> GH.repoDefaultBranch branch

doesGhFileExist :: (HasCallStack) => RepoOwner -> RepoName -> ForgeToken -> CommitHash -> FilePath -> M DoesFileExist
doesGhFileExist (RepoOwner (ForgeLogin owner')) (RepoName repoName') token (CommitHash commit') path = do
  let url =
        "https://api.github.com/repos"
          </> cs owner'
          </> cs repoName'
          </> "contents"
          </> path
          <> "?ref="
          <> cs commit'
  response <-
    retryWreq $ withWreqOptions $ \options ->
      Wreq.headWith
        (options & Wreq.auth ?~ Wreq.oauth2Token (cs $ getForgeToken token) & Wreq.checkResponse ?~ \_ _ -> pure ())
        url
  maybe FileDoesntExist (const FileExists) <$> handleGithubWreqErrors "doesGhFileExist" response

ghRemoteUrl :: RepoOwner -> RepoName -> ForgeToken -> Maybe PrFromFork -> RemoteUrl
ghRemoteUrl (RepoOwner (ForgeLogin owner')) (RepoName repo') token = \case
  Just (PrFromFork fromFork) ->
    RemoteUrl $ "https://github.com/" <> fromFork <> ".git"
  Nothing ->
    RemoteUrl $ "https://x-access-token:" <> getForgeToken token <> "@github.com/" <> owner' <> "/" <> repo' <> ".git"

getRepoCollaboratorsGH :: (HasCallStack) => GHA.InstallationAuth -> RepoOwner -> RepoName -> M Collaborators
getRepoCollaboratorsGH iAuth owner@(RepoOwner (ForgeLogin repoOwner)) repo@(RepoName repoName) = do
  let req = GH.collaboratorsOnR (GH.mkName Proxy repoOwner) (GH.mkName Proxy repoName) GH.FetchAll
  executeAppRequest iAuth req
    >>= handleGithubRequestErrors "getRepoCollaborators" owner repo
    >>= \collaborators ->
      pure $ Collaborators $ ForgeLogin . GH.untagName . GH.simpleUserLogin <$> Vector.toList collaborators

getRepoPublicityGH :: (HasCallStack) => GHA.InstallationAuth -> RepoOwner -> RepoName -> M RepoPublicity
getRepoPublicityGH iAuth owner@(RepoOwner (ForgeLogin repoOwner)) repo@(RepoName repoName) = do
  let req = GH.repositoryR (GH.mkName Proxy repoOwner) (GH.mkName Proxy repoName)
  executeAppRequest iAuth req
    >>= handleGithubRequestErrors "getRepoPublicity" owner repo
    >>= \r -> pure $ RepoIsPublic $ not $ GH.repoPrivate r

-- | Build a 'RepoInfo' for a GitHub repository from its installation auth and
-- token. The single place that pairs the GitHub forge with GitHub credentials when
-- constructing repo context (webhooks, orchestrator, etc.).
githubRepoInfo :: GHA.InstallationAuth -> ForgeToken -> RepoOwner -> RepoName -> RepoInfo
githubRepoInfo iAuth token owner name =
  RepoInfo (SomeForgeRepo (ForgeRepo delegatingGithubForge (GithubAuth iAuth token) owner name)) owner name

githubForge :: (HasCallStack) => Forge 'GitHub
githubForge =
  Forge
    { _forgeForgeKind = SGitHub,
      _forgeGetInstallation = \fid -> do
        appAuth <- githubAppConfigAuth <$> githubAppConfig
        iAuth <- liftIO $ GHA.mkInstallationAuth appAuth (ghInstallationId fid)
        token <- obtainGhToken iAuth
        pure $ GithubAuth iAuth token,
      _forgeGetAppInstallationId = \owner name ->
        fmap ForgeInstallationId <$> garnixInstallationIdGH owner name,
      _forgeGetAccessToken = \(GithubAuth iAuth _) -> obtainGhToken iAuth,
      _forgeGetDefaultBranch = \mAuth owner repo ->
        getDefaultBranchGH ((\(GithubAuth iAuth _) -> iAuth) <$> mAuth) owner repo,
      _forgeGetHeadCommit = getHeadCommitForBranch,
      _forgeNewBuildReport = \(ForgeRepo _ (GithubAuth iAuth _) owner name) report ->
        createBuildReportGH iAuth owner name report,
      _forgeUpdateBuildReport = \runId report (ForgeRepo _ (GithubAuth iAuth _) owner name) ->
        updateBuildReportGH runId report iAuth owner name,
      _forgeDoesRepoFileExist = \frepo commit _mFork path ->
        doesGhFileExist (_forgeRepoOwner frepo) (_forgeRepoName frepo) (forgeAuthToken (_forgeRepoAuth frepo)) commit path,
      _forgeGetRemote = \frepo _commit mFork ->
        pure $ ghRemoteUrl (_forgeRepoOwner frepo) (_forgeRepoName frepo) (forgeAuthToken (_forgeRepoAuth frepo)) mFork,
      _forgeGetRepoCollaborators = \(GithubAuth iAuth _) owner repo -> getRepoCollaboratorsGH iAuth owner repo,
      _forgeGetRepoPublicity = \(GithubAuth iAuth _) owner repo -> getRepoPublicityGH iAuth owner repo,
      _forgeGetInstallations = \token ->
        map (ForgeInstallationId . toInteger . GH.untagId) <$> getInstallations token,
      _forgeGetInstalledOrgs = getInstalledOrgs,
      _forgeGetReposAccessibleTo = \fid token -> getReposInInstallationAccessibleTo (ghInstallationId fid) token,
      _forgeOpenPullRequest = openGithubPullRequestInternal
    }
  where
    ghInstallationId :: ForgeInstallationId -> Id GHA.Installation
    ghInstallationId = Id . fromInteger . getForgeInstallationId
