{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Garnix.Monad
  ( module Garnix.Monad,
    Severity (..),
  )
where

import Amazonka.Env qualified as Amazonka (Env)
import Amazonka.S3 qualified as Amazonka
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception.Safe qualified as SafeException
import Control.Lens (IndexedTraversal')
import Control.Lens.Regex.Text qualified as RE
import Control.Monad.Base (MonadBase)
import Data.Aeson (Value, eitherDecode')
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Containers.ListUtils (nubOrd)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Format.Numbers (PrettyCfg (PrettyCfg), prettyF)
import Data.Time (Day)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.UUID qualified
import Data.UUID.V4 qualified
import Garnix.Async (Promise)
import Garnix.Build.Types (EvaluationResult)
import Garnix.BuildLogs.Types (LogLine)
import Garnix.DB.FeatureFlags.Types (FeatureFlagConfig)
import Garnix.Duration
import Garnix.GithubInterface.Types
import Garnix.Hosting.ServerPool.Types
import Garnix.Log
import Garnix.Monad.ForkT
import Garnix.Monad.Memoization (MemoTable)
import Garnix.Monad.Metrics (Metrics, incrementEvent)
import Garnix.Forge.Types
import Garnix.Monad.Pool (Pool)
import Garnix.Nix.Types (StoreHash)
import Garnix.Nix.Types qualified as Nix
import Garnix.Prelude
import Garnix.StripeLib.Types qualified as StripeLib
import Garnix.Types hiding (ghRunId, statusCode)
import GitHub qualified as GH
import GitHub.App.Auth qualified as GHA
import GitHub.Data.Installations qualified as GHA
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (statusCode)
import Network.Wai qualified as Wai
import Network.Wai.Header (contentLength)
import Network.Wreq qualified as Wreq
import Servant.Auth.Server (CookieSettings, JWTSettings)
import System.Directory (canonicalizePath)
import System.Log.FastLogger as FastLogger
import Text.Read (readMaybe)

data Env = Env
  { testFeatures :: Set TestFeature,
    -- | Per-forge configuration\/secrets; absent for unconfigured forges.
    forgeConfigs :: ForgeKind -> Maybe ForgeConfig,
    manager :: Manager,
    buildLogsReportingPort :: Maybe Int,
    forges :: ForgeKind -> SomeForge,
    hetznerInterface :: HetznerInterface,
    serverPoolConfig :: [(ServerTier, Int)],
    -- | A thread-safe version of `CWD`
    workingDir :: FilePath,
    nixXdgCacheDir :: Maybe FilePath,
    userNixConfig :: NixConfig,
    cookieSettings :: CookieSettings,
    jwtSettings :: JWTSettings,
    dbConn :: DatabaseConnection,
    baseUrl :: Text,
    sshUserHostingKeys :: [FilePath],
    s3CacheEnv :: S3CacheEnv,
    action :: ActionEnv,
    repoSecretsEncryptionKeyPath :: RepoSecretsEncryptionKeyPath,
    repoSecretsEncryptionPubKey :: RepoSecretsEncryptionPubKey,
    logger :: LogItem -> IO (),
    buildLogsDir :: FilePath,
    hetznerToken :: StrictByteString,
    opensearchQueryUrl :: String,
    opensearchPassword :: ByteString,
    nixEvalPool :: Garnix.Monad.Pool.Pool RepoOwner,
    s3UploadPool :: Garnix.Monad.Pool.Pool RepoOwner,
    stripe :: StripeEnv,
    mocks :: Maybe EnvMocks,
    emptyDir :: FilePath,
    spanCtx :: [(Text, Text)],
    metrics :: Metrics,
    hostname :: Text,
    githubLogDebounceDuration :: Duration,
    featureFlagConfig :: FeatureFlagConfig,
    fodCheckPool :: Garnix.Monad.Pool.Pool ()
  }
  deriving stock (Generic)

data TestFeature
  = DevApi
  | OpenSearchMocks
  | StripeMocks
  | CacheUploadMocks
  | FodCheckMocks
  deriving stock (Eq, Show, Read, Ord, Generic, Enum, Bounded)

localDevelopment :: Set TestFeature
localDevelopment = Set.fromList [minBound .. maxBound]

parseTestFeature :: String -> Either Text TestFeature
parseTestFeature feature = case readMaybe feature of
  Just feature -> Right feature
  Nothing ->
    Left
      $ "unknown test feature passed to --enable: "
      <> cs feature
      <> ". Possible values: "
      <> T.intercalate ", " (map show [minBound .. maxBound :: TestFeature])

data S3CacheEnv = S3CacheEnv
  { amazonkaEnv :: Amazonka.Env,
    publicBucket :: Amazonka.BucketName,
    publicBaseUrl :: Text,
    privateBucket :: Amazonka.BucketName,
    cachePrivKeyFile :: FilePath,
    cachePrivKeyName :: Text,
    expiration :: Duration,
    maxUploadSize :: Integer,
    isInNixosCacheMemoTable :: MVar (MemoTable StoreHash Bool)
  }
  deriving (Generic)

data ActionEnv = ActionEnv
  { runnerHost :: Text,
    runnerSshKey :: Text,
    timeoutDuration :: Duration
  }
  deriving (Generic)

data StripeEnv = StripeEnv
  { publishableKey :: Text,
    secretKey :: Text,
    webhookSecret :: Text
  }
  deriving stock (Generic)

data EnvMocks = EnvMocks
  { executeDeployPlanMock ::
      Maybe
        (Mock (Reporter, CommitInfo, DeployPlan, DeploymentType) [ServerInfo]),
    waitTillServerIsInitializedMock :: Maybe (Mock HetznerServerId Bool),
    buildFlakeMock ::
      Maybe
        (Mock (Reporter, CommitInfo) (Promise ())),
    storeLogLineMock :: Maybe (Mock (OpenSearchId, LogLine) ()),
    queryOpenSearchMock ::
      Maybe
        (Mock (OpenSearchId, [Day], Maybe UTCTime, Int) [OpenSearchMessage]),
    startServerMock ::
      Maybe
        (Mock (Reporter, CommitInfo, DeploymentType, ServerToSpinUp) ServerInfo),
    setupServerMock ::
      Maybe
        (Mock (RepoInfo, Build, ServerInfo) (ServerInfo, Text)),
    makeOpenSearchMsearchRequestMock ::
      Maybe
        (Mock (Value, Value) BSL.ByteString),
    createCustomerMock :: Maybe (Mock (RepoOwner, StripeLib.Name, Email) StripeLib.CustomerDto),
    createSubscriptionMock :: Maybe (Mock (CustomerId, StripeLib.PriceId, Text, Text) StripeLib.SubscriptionDto),
    createInvoiceItemMock :: Maybe (Mock (CustomerId, InvoiceId, Text, StripeLib.UnitAmount, Int64) ()),
    listSubscriptionsMock :: Maybe (Mock CustomerId StripeLib.SubscriptionListDto),
    cancelSubscriptionMock :: Maybe (Mock SubscriptionId ()),
    getPriceMock :: Maybe (Mock StripeLib.PriceId StripeLib.PriceDto),
    getBuildPlanMock :: Maybe (Mock ByteString Nix.Plan),
    buildPkgMock :: Maybe (Mock (Maybe FodChecker, RunReporter, BuildKind, FlakeDir, RepoConfig, ProductPlan, Build) Build),
    s3CacheUploadMock :: Maybe (Mock (RunReporter, RepoOwner, RepoName, EvaluationResult, RepoPublicity) ()),
    fodCheckMock :: Maybe (Mock (Maybe FodChecker, Nix.DrvPath) ()),
    rebuildFodMock :: Maybe (Mock (System, Nix.DrvPath) (Either Text Text))
  }
  deriving (Generic)

emptyMocks :: EnvMocks
emptyMocks =
  EnvMocks
    { executeDeployPlanMock = Nothing,
      waitTillServerIsInitializedMock = Nothing,
      buildFlakeMock = Nothing,
      storeLogLineMock = Nothing,
      queryOpenSearchMock = Nothing,
      startServerMock = Nothing,
      makeOpenSearchMsearchRequestMock = Nothing,
      createCustomerMock = Nothing,
      createSubscriptionMock = Nothing,
      createInvoiceItemMock = Nothing,
      listSubscriptionsMock = Nothing,
      cancelSubscriptionMock = Nothing,
      getPriceMock = Nothing,
      getBuildPlanMock = Nothing,
      setupServerMock = Nothing,
      buildPkgMock = Nothing,
      s3CacheUploadMock = Nothing,
      fodCheckMock = Nothing,
      rebuildFodMock = Nothing
    }

data Mock arg result = Mock
  { mockImplementation :: arg -> M result,
    calls :: MVar [arg]
  }

relativeUrlConverter :: M (Text -> Text)
relativeUrlConverter = do
  burl <- asks baseUrl
  pure $ \end ->
    if
      | "/" `T.isPrefixOf` end && "/" `T.isSuffixOf` burl -> burl <> T.drop 1 end
      | "/" `T.isPrefixOf` end || "/" `T.isSuffixOf` burl -> burl <> end
      | otherwise -> burl <> "/" <> end

newtype M a = M {runM' :: ExceptT ErrorWithContext (ReaderT Env (ForkT IO)) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError ErrorWithContext,
      MonadIO,
      MonadBase IO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadBaseControl IO,
      HasForkT
    )

runM :: Env -> M a -> IO (Either ErrorWithContext a)
runM env (M action) = runForkT (runReaderT (runExceptT action) env)

newtype FlakeDir = FlakeDir {__unsafeGetFlakeDir :: FilePath}
  deriving newtype (Eq, Show, Generic)

newtype ReportSummary = ReportSummary {getReportSummary :: Text}
  deriving stock (Show, Eq)

data RunReporter = RunReporter
  { reportLogs :: LogLine -> M (),
    reportComplete :: RunReportStatus -> M ()
  }

instance Semigroup RunReporter where
  a <> b =
    RunReporter
      { reportLogs = \logs -> reportLogs a logs >> reportLogs b logs,
        reportComplete = \status -> reportComplete a status >> reportComplete b status
      }

instance Monoid RunReporter where
  mempty =
    RunReporter
      { reportLogs = \_ -> pure (),
        reportComplete = \_ -> pure ()
      }

data ReportType
  = ReportBuild {_reportTypeName :: Text, _reportTypeBuild :: Build}
  | ReportRun {_reportTypeRun :: Run}
  | MetaCheck
  deriving stock (Show, Eq)

reportName :: ReportType -> Text
reportName = \case
  ReportBuild name _ -> name
  ReportRun run -> run ^. name
  MetaCheck -> "All Garnix checks"

newtype Reporter = Reporter {createNewRun :: ReportType -> M RunReporter}
  deriving stock (Show)

instance Semigroup Reporter where
  a <> b =
    Reporter
      { createNewRun = \reportType ->
          (<>) <$> createNewRun a reportType <*> createNewRun b reportType
      }

instance Monoid Reporter where
  mempty = Reporter {createNewRun = \_ -> pure mempty}

data FodChecker = FodChecker
  { runReporter :: RunReporter,
    totalSkipped :: MVar Int,
    totalVerified :: MVar Int,
    promises :: MVar (Maybe [Promise (Either [(Nix.DrvPath, Text)] ())]),
    startedOrDone :: MVar (Set Nix.DrvPath)
  }
  deriving (Generic)

-- * Github interface

data Collaborators
  = Collaborators [ForgeLogin]
  | RepoNotFound
  deriving (Show)

newtype RemoteUrl = RemoteUrl Text

realRemoteUrl :: RemoteUrl -> Text
realRemoteUrl (RemoteUrl url) = url

-- * Forge interface (generalizes the old @GithubInterface@ over 'ForgeKind')

-- | The generalized, kind-indexed forge interface. It mirrors 'GithubInterface'
-- but is parameterized by the forge kind @k@: every method touching credentials
-- uses @'ForgeAuth' k@, so an implementation can only ever be fed its own
-- credentials. A concrete forge ('githubForge', 'giteaForge', …) fixes @k@.
--
-- Repo context is passed as a 'ForgeRepo' @k@, which bundles the forge, its
-- matching credentials, and the owner\/name — all sharing the index @k@ — so the
-- credentials can never be paired with the wrong forge.
data Forge (k :: ForgeKind) = Forge
  { _forgeForgeKind :: SForgeKind k,
    _forgeGetInstallation :: (HasCallStack) => ForgeInstallationId -> M (ForgeAuth k),
    _forgeGetAppInstallationId :: (HasCallStack) => RepoOwner -> RepoName -> M (Maybe ForgeInstallationId),
    _forgeGetAccessToken :: (HasCallStack) => ForgeAuth k -> M ForgeToken,
    _forgeGetDefaultBranch :: (HasCallStack) => Maybe (ForgeAuth k) -> RepoOwner -> RepoName -> M (Maybe Branch),
    _forgeGetHeadCommit :: (HasCallStack) => ForgeToken -> RepoOwner -> RepoName -> Branch -> M CommitHash,
    _forgeNewBuildReport :: (HasCallStack) => ForgeRepo k -> GhRunReport -> M ForgeRunId,
    _forgeUpdateBuildReport :: (HasCallStack) => ForgeRunId -> GhRunReport -> ForgeRepo k -> M (),
    _forgeDoesRepoFileExist :: (HasCallStack) => ForgeRepo k -> CommitHash -> Maybe PrFromFork -> FilePath -> M DoesFileExist,
    _forgeGetRemote :: (HasCallStack) => ForgeRepo k -> CommitHash -> Maybe PrFromFork -> M RemoteUrl,
    _forgeGetRepoCollaborators :: (HasCallStack) => ForgeAuth k -> RepoOwner -> RepoName -> M Collaborators,
    _forgeGetRepoPublicity :: (HasCallStack) => ForgeAuth k -> RepoOwner -> RepoName -> M RepoPublicity,
    _forgeGetInstallations :: (HasCallStack) => ForgeToken -> M [ForgeInstallationId],
    _forgeGetInstalledOrgs :: (HasCallStack) => ForgeToken -> M [UserOrgMembership],
    _forgeGetReposAccessibleTo :: (HasCallStack) => ForgeInstallationId -> ForgeToken -> M [Text],
    _forgeOpenPullRequest :: (HasCallStack) => RepoOwner -> RepoName -> PullRequest -> M PullRequestResult
  }

-- | A repository together with the forge it lives on and a /matching/ credential.
-- All three share the index @k@, so 'forgeRepoAuth' can only be used with
-- 'forgeRepoForge'. This is the value that flows through the build pipeline
-- (eventually replacing 'RepoInfo').
data ForgeRepo (k :: ForgeKind) = ForgeRepo
  { _forgeRepoForge :: Forge k,
    _forgeRepoAuth :: ForgeAuth k,
    _forgeRepoOwner :: RepoOwner,
    _forgeRepoName :: RepoName
  }

-- | A forge with its kind index hidden — used for the registry and for
-- reconstructing a forge from a runtime 'ForgeKind' at the one existential
-- boundary (webhook ingress \/ DB load).
data SomeForge where
  SomeForge :: Forge k -> SomeForge

-- | A 'ForgeRepo' with its kind index hidden, for code paths that handle a repo of
-- statically-unknown forge (e.g. a generic DB listing). Unpack once at the top.
data SomeForgeRepo where
  SomeForgeRepo :: ForgeRepo k -> SomeForgeRepo

-- | A repository plus the forge it lives on and matching credentials, bundled
-- type-safely in 'SomeForgeRepo'. (Moved here from "Garnix.Types" so it can carry
-- the M-valued forge.) The owner\/name are duplicated at the top level for the many
-- existing @^. ghRepoOwner@ \/ @^. ghRepoName@ call sites; they always match the
-- bundle.
data RepoInfo = RepoInfo
  { _repoInfoForgeRepo :: SomeForgeRepo,
    _repoInfoGhRepoOwner :: RepoOwner,
    _repoInfoGhRepoName :: RepoName
  }

instance Show RepoInfo where
  showsPrec _ (RepoInfo _ owner name) =
    showString $ "RepoInfo <forge> " <> cs (show owner) <> " " <> cs (show name)

-- | The access token carried by a repo's forge credentials.
repoInfoToken :: RepoInfo -> ForgeToken
repoInfoToken (RepoInfo (SomeForgeRepo (ForgeRepo _ auth _ _)) _ _) = forgeAuthToken auth

data CommitInfo = CommitInfo
  { _commitInfoReqUser :: ForgeLogin,
    _commitInfoRepoPublicity :: RepoPublicity,
    _commitInfoRepoInfo :: RepoInfo,
    _commitInfoBranch :: Maybe Branch,
    _commitInfoPrFromFork :: Maybe PrFromFork,
    _commitInfoCommit :: CommitHash
  }
  deriving stock (Show)

data RunReportStatus
  = RunReportStatusInProgress
  | RunReportStatusSuccess
  | RunReportStatusFailure
  | RunReportStatusTimeout
  | RunReportStatusCancelled
  deriving stock (Eq, Show)

data GhRunReport = GhRunReport
  { _ghRunReportName :: Text,
    _ghRunReportCommit :: CommitHash,
    _ghRunReportUrl :: Maybe Text,
    _ghRunReportStatus :: RunReportStatus,
    _ghRunReportTitle :: Text,
    _ghRunReportSummary :: Text,
    _ghRunReportLogs :: RawLogs,
    -- | Our own correlation key (the build id), set as the check run's
    -- @external_id@ so a provider-native rerun can be mapped back to a build
    -- without us storing the forge's run id.
    _ghRunReportExternalId :: Maybe Text
  }
  deriving stock (Eq, Show)

data DeploymentStatus = DeploymentInProgress | DeploymentSuccess | DeploymentFailure

data PullRequest = PullRequest
  { _pullRequestTitle :: Text,
    _pullRequestBody :: Text,
    _pullRequestHeadBranch :: Branch,
    _pullRequestBaseBranch :: Branch
  }

-- * HetznerInterface

data HetznerInterface = HetznerInterface
  { _hetznerInterfaceProvisionServer :: PreprovisionedServerId -> HetznerLocation -> HetznerServerType -> M PreprovisionedServer,
    _hetznerInterfaceUpdateMetadata :: RepoInfo -> DeploymentType -> Build -> ServerId -> HetznerServerId -> M (),
    _hetznerInterfaceDeleteServer :: HetznerServerId -> M (),
    _hetznerInterfaceGetServerStatus :: HetznerServerId -> M Text
  }

makeFields ''PullRequest
makeFields ''EnvMocks
makeFields ''GhRunReport
makeFields ''RepoInfo
makeFields ''CommitInfo
makePrisms ''CommitInfo

-- 'Loggable' instances for the moved types (the class lives in "Garnix.Log", which
-- this module imports; the instances are non-orphan because the types are here).
instance Loggable CommitInfo where
  asLog info = asLog (info ^. _CommitInfo)

instance Loggable RepoInfo where
  asLog (RepoInfo _ owner name) = asLog owner <> asLog name

-- Accessors

-- | Resolve the GitHub forge from the registry, for the GitHub-app-specific
-- operations (installations, app auth, the logged-in user's orgs\/repos) that have
-- no cross-forge meaning. The 'SGitHub' match recovers @k ~ 'GitHub@.
withGithubForge :: (HasCallStack) => (Forge 'GitHub -> M a) -> M a
withGithubForge g = do
  forges' <- view #forges
  case forges' GitHub of
    SomeForge f -> case _forgeForgeKind f of
      SGitHub -> g f
      _ -> throw $ OtherError "withGithubForge: registry returned a non-GitHub forge for GitHub"

-- | Run an action with the forge and matching credentials bundled in a 'RepoInfo'.
withRepoForge :: RepoInfo -> (forall k. Forge k -> ForgeRepo k -> M a) -> M a
withRepoForge repoInfo g =
  case _repoInfoForgeRepo repoInfo of
    SomeForgeRepo fr -> g (_forgeRepoForge fr) fr

-- | A GitHub 'Forge' whose every method delegates to whatever GitHub forge is
-- currently in 'Env.forges'. This is what 'RepoInfo' bundles carry, so that the
-- single source of truth for the GitHub implementation is the registry (the real
-- forge in production, an injected fake in tests) — while the bundle still pairs a
-- @Forge 'GitHub@ with @ForgeAuth 'GitHub@ for type safety.
delegatingGithubForge :: Forge 'GitHub
delegatingGithubForge =
  Forge
    { _forgeForgeKind = SGitHub,
      _forgeGetInstallation = \a -> withGithubForge $ \f -> _forgeGetInstallation f a,
      _forgeGetAppInstallationId = \a b -> withGithubForge $ \f -> _forgeGetAppInstallationId f a b,
      _forgeGetAccessToken = \a -> withGithubForge $ \f -> _forgeGetAccessToken f a,
      _forgeGetDefaultBranch = \a b c -> withGithubForge $ \f -> _forgeGetDefaultBranch f a b c,
      _forgeGetHeadCommit = \a b c d -> withGithubForge $ \f -> _forgeGetHeadCommit f a b c d,
      _forgeNewBuildReport = \a b -> withGithubForge $ \f -> _forgeNewBuildReport f a b,
      _forgeUpdateBuildReport = \a b c -> withGithubForge $ \f -> _forgeUpdateBuildReport f a b c,
      _forgeDoesRepoFileExist = \a b c d -> withGithubForge $ \f -> _forgeDoesRepoFileExist f a b c d,
      _forgeGetRemote = \a b c -> withGithubForge $ \f -> _forgeGetRemote f a b c,
      _forgeGetRepoCollaborators = \a b c -> withGithubForge $ \f -> _forgeGetRepoCollaborators f a b c,
      _forgeGetRepoPublicity = \a b c -> withGithubForge $ \f -> _forgeGetRepoPublicity f a b c,
      _forgeGetInstalledOrgs = \a -> withGithubForge $ \f -> _forgeGetInstalledOrgs f a,
      _forgeGetInstallations = \a -> withGithubForge $ \f -> _forgeGetInstallations f a,
      _forgeGetReposAccessibleTo = \a b -> withGithubForge $ \f -> _forgeGetReposAccessibleTo f a b,
      _forgeOpenPullRequest = \a b c -> withGithubForge $ \f -> _forgeOpenPullRequest f a b c
    }

-- | The configuration for a forge, or an error if it isn't configured.
forgeConfig :: (HasCallStack) => ForgeKind -> M ForgeConfig
forgeConfig kind = do
  configs <- view #forgeConfigs
  case configs kind of
    Just c -> pure c
    Nothing -> throw $ OtherError $ "No configuration for forge " <> show kind

-- | The GitHub App configuration (GitHub must be configured with an app).
githubAppConfig :: (HasCallStack) => M GithubAppConfig
githubAppConfig = do
  c <- forgeConfig GitHub
  case forgeConfigApp c of
    Just app -> pure app
    Nothing -> throw $ OtherError "GitHub forge is configured without a GitHub App"

getInstallation :: GH.Id GHA.Installation -> M GHA.InstallationAuth
getInstallation inst = withGithubForge $ \f -> do
  auth <- _forgeGetInstallation f (ForgeInstallationId (toInteger (GH.untagId inst)))
  case auth of GithubAuth iAuth _ -> pure iAuth

getInstallations :: ForgeToken -> M [GH.Id GHA.Installation]
getInstallations token = withGithubForge $ \f ->
  map (GH.mkId Proxy . fromInteger . getForgeInstallationId) <$> _forgeGetInstallations f token

getGarnixInstallationId :: RepoOwner -> RepoName -> M (Maybe Integer)
getGarnixInstallationId owner name = withGithubForge $ \f ->
  fmap (toInteger . getForgeInstallationId) <$> _forgeGetAppInstallationId f owner name

getAccessToken :: GHA.InstallationAuth -> M ForgeToken
getAccessToken iAuth = withGithubForge $ \f -> _forgeGetAccessToken f (GithubAuth iAuth (ForgeToken ""))

getDefaultBranch :: Maybe GHA.InstallationAuth -> RepoOwner -> RepoName -> M (Maybe Branch)
getDefaultBranch miAuth owner repo = withGithubForge $ \f ->
  _forgeGetDefaultBranch f ((\ia -> GithubAuth ia (ForgeToken "")) <$> miAuth) owner repo

getHeadCommit :: ForgeToken -> RepoOwner -> RepoName -> Branch -> M CommitHash
getHeadCommit token owner repo branch = withGithubForge $ \f -> _forgeGetHeadCommit f token owner repo branch

newBuildReport :: RepoInfo -> GhRunReport -> M ForgeRunId
newBuildReport repoInfo build' = withRepoForge repoInfo $ \f fr -> _forgeNewBuildReport f fr build'

updateBuildReport :: ForgeRunId -> GhRunReport -> RepoInfo -> M ()
updateBuildReport runId' runReport repoInfo = withRepoForge repoInfo $ \f fr -> _forgeUpdateBuildReport f runId' runReport fr

getRemote :: (HasCallStack) => CommitInfo -> M RemoteUrl
getRemote commitInfo = withRepoForge (commitInfo ^. repoInfo) $ \f fr ->
  _forgeGetRemote f fr (commitInfo ^. commit) (commitInfo ^. prFromFork)

-- | The GitHub installation auth embedded in a repo's forge bundle (GitHub only).
repoInfoGithubAuth :: (HasCallStack) => RepoInfo -> M GHA.InstallationAuth
repoInfoGithubAuth (RepoInfo (SomeForgeRepo (ForgeRepo _ auth _ _)) _ _) =
  case auth of
    GithubAuth iAuth _ -> pure iAuth
    _ -> throw $ OtherError "repoInfoGithubAuth: repo is not on GitHub"

getRepoCollaborators :: (HasCallStack) => GHA.InstallationAuth -> RepoOwner -> RepoName -> M Collaborators
getRepoCollaborators iAuth owner repo = withGithubForge $ \f ->
  _forgeGetRepoCollaborators f (GithubAuth iAuth (ForgeToken "")) owner repo

doesRepoFileExist :: (HasCallStack) => CommitInfo -> FilePath -> M DoesFileExist
doesRepoFileExist commitInfo path = withRepoForge (commitInfo ^. repoInfo) $ \f fr ->
  _forgeDoesRepoFileExist f fr (commitInfo ^. commit) (commitInfo ^. prFromFork) path

getRepoPublicity :: (HasCallStack) => GHA.InstallationAuth -> RepoOwner -> RepoName -> M RepoPublicity
getRepoPublicity iAuth owner name = withGithubForge $ \f ->
  _forgeGetRepoPublicity f (GithubAuth iAuth (ForgeToken "")) owner name

getInstalledOrgs :: (HasCallStack) => ForgeToken -> M [UserOrgMembership]
getInstalledOrgs tok = withGithubForge $ \f -> _forgeGetInstalledOrgs f tok

getReposInInstallationAccessibleTo :: (HasCallStack) => GH.Id GHA.Installation -> ForgeToken -> M [Text]
getReposInInstallationAccessibleTo installation token = withGithubForge $ \f ->
  _forgeGetReposAccessibleTo f (ForgeInstallationId (toInteger (GH.untagId installation))) token

openGithubPullRequest :: (HasCallStack) => RepoOwner -> RepoName -> PullRequest -> M PullRequestResult
openGithubPullRequest owner name pr = withGithubForge $ \f -> _forgeOpenPullRequest f owner name pr

withWreqOptions :: (Wreq.Options -> IO a) -> M a
withWreqOptions action = do
  manager <- view #manager
  let options = Wreq.defaults & Wreq.manager .~ Right manager
  liftIO $ action options

provisionServer :: PreprovisionedServerId -> HetznerLocation -> HetznerServerType -> M PreprovisionedServer
provisionServer sId loc typ = do
  iface <- view #hetznerInterface
  _hetznerInterfaceProvisionServer iface sId loc typ

updateMetadata :: RepoInfo -> DeploymentType -> Build -> ServerId -> HetznerServerId -> M ()
updateMetadata repoInfo deploymentType build serverId hetznerServerId = do
  iface <- view #hetznerInterface
  _hetznerInterfaceUpdateMetadata iface repoInfo deploymentType build serverId hetznerServerId

deleteServer :: HetznerServerId -> M ()
deleteServer sId = do
  iface <- view #hetznerInterface
  _hetznerInterfaceDeleteServer iface sId

getServerStatus :: HetznerServerId -> M Text
getServerStatus sId = do
  iface <- view #hetznerInterface
  _hetznerInterfaceGetServerStatus iface sId

getNixXdgCacheDir :: M String
getNixXdgCacheDir =
  view #nixXdgCacheDir
    >>= maybe (throw $ OtherError "Local cache not properly set up.") pure

newtype RequestTraceId = RequestTraceId {getRequestTraceId :: Data.UUID.UUID}

instance Loggable RequestTraceId where
  asLog (RequestTraceId uuid) = [("request_trace_id", Data.UUID.toText uuid)]

-- * mocking

newMock :: (a -> M b) -> IO (Mock a b)
newMock f = do
  Mock f <$> newMVar []

mockable ::
  Lens' EnvMocks (Maybe (Mock arg result)) ->
  (arg -> M result) ->
  (arg -> M result)
mockable lens prodImplementation arg = do
  mocks <- view #mocks
  case mocks >>= (^. lens) of
    Nothing -> prodImplementation arg
    Just (Mock mock calls) -> do
      liftIO $ modifyMVar_ calls $ \acc -> pure (acc ++ [arg])
      mock arg

withMock ::
  Lens' EnvMocks (Maybe (Mock arg result)) ->
  (arg -> M result) ->
  M a ->
  M a
withMock lens mock action = do
  mocks <- view #mocks
  case mocks of
    Nothing -> log Critical "trying to mock during production"
    Just _ -> pure ()
  calls <- liftIO $ newMVar []
  local (setMock calls) action
  where
    setMock calls env =
      env & #mocks . _Just . lens ?~ Mock mock calls

withUnmock :: Lens' EnvMocks (Maybe (Mock arg result)) -> M a -> M a
withUnmock lens action = do
  local unsetMock action
  where
    unsetMock env = case mocks env of
      Nothing -> env
      Just mocks ->
        env {mocks = Just (mocks & lens .~ Nothing)}

withMockReturning ::
  Lens' EnvMocks (Maybe (Mock arg result)) ->
  result ->
  M a ->
  M a
withMockReturning lens result =
  withMock lens $ const $ pure result

getMockCalls ::
  Lens' EnvMocks (Maybe (Mock arg result)) ->
  M [arg]
getMockCalls lens = do
  mocks <- view #mocks
  case mocks >>= (^. lens) of
    Just mock -> liftIO $ readMVar $ calls mock
    Nothing -> throw $ OtherError "getMockCalls called for unmocked function"

-- * logging

withDefaultLogger :: ((LogItem -> IO ()) -> IO a) -> IO a
withDefaultLogger action =
  bracket
    (FastLogger.newFastLogger1 $ FastLogger.LogStdout FastLogger.defaultBufSize)
    snd
    (\(logger, _) -> action (\x -> logger $ FastLogger.toLogStr x <> "\n"))

log :: Severity -> Text -> M ()
log sev txt = do
  spans <- asks spanCtx
  logger <- asks logger
  liftIO $ logger (LogItem sev (nubOrd spans) (limitMessage txt))
  case sev of
    Critical -> incrementEvent #logsCritical
    Error -> incrementEvent #logsError
    Warning -> incrementEvent #logsWarning
    _ -> pure ()
  where
    limit = 20000
    limitMessage text =
      if T.length text > limit
        then T.take limit text <> "...[snip]"
        else text

logRequestsMiddleware :: Env -> (RequestTraceId -> Wai.Application) -> Wai.Application
logRequestsMiddleware env innerApp request respond = do
  requestTraceId <- RequestTraceId <$> liftIO Data.UUID.V4.nextRandom
  innerApp requestTraceId request $ \response -> do
    result <- respond response
    void $ runM env $ do
      let spans =
            [ ("method", cs $ Wai.requestMethod request),
              ("path", cs $ Wai.rawPathInfo request),
              ("http_version", show $ Wai.httpVersion request),
              ("status", show $ statusCode $ Wai.responseStatus response),
              ("response_content_length", maybe "-" show $ contentLength $ Wai.responseHeaders response),
              ("referrer", maybe "-" cs $ Wai.requestHeaderReferer request),
              ("user_agent", maybe "-" cs $ Wai.requestHeaderUserAgent request)
            ]
      withSpan requestTraceId
        $ withTextSpans spans
        $ do
          log Informational "http request"
    pure result

withSpan :: (Loggable l) => l -> M a -> M a
withSpan toAdd = withTextSpans (asLog toAdd)

withTextSpan :: (Text, Text) -> M a -> M a
withTextSpan toAdd = withTextSpans [toAdd]

withTextSpans :: [(Text, Text)] -> M a -> M a
withTextSpans toAdd = local $ \env -> env {spanCtx = map addSpanPrefix toAdd <> spanCtx env}
  where
    addSpanPrefix :: (Text, Text) -> (Text, Text)
    addSpanPrefix = first ("span_" <>)

logThrownErrors :: M a -> M a
logThrownErrors action = action `catchError` (\e -> logError e >> rethrow e)

logError :: ErrorWithContext -> M ()
logError error = withRawTextSpans (spans error) $ do
  log (error ^. #severity) $ showDebug error
  where
    withRawTextSpans :: [(Text, Text)] -> M a -> M a
    withRawTextSpans rawSpans = local $ \env -> env {spanCtx = rawSpans <> spanCtx env}

logSomeException :: SomeException -> M ()
logSomeException error =
  log Error $ "runtime exception: " <> show error

(<?>) :: M a -> Text -> M a
action <?> msg = do
  log Notice msg
  result <-
    action `whenErrorEither` \e -> do
      log Warning (msg <> " - FAILED: " <> either show showDebug e)
  log Notice (msg <> " - DONE")
  pure result

withMessage :: Text -> M a -> M a
withMessage = flip (<?>)

logDuration :: Text -> M a -> M a
logDuration message action = do
  before <- liftIO getSystemTime
  result <- action
  after <- liftIO getSystemTime
  let seconds = nominalDiffTimeToSeconds $ diffUTCTime (systemToUTCTime after) (systemToUTCTime before)
  log Informational $ "logDuration: " <> message <> ", seconds: " <> prettyF (PrettyCfg 2 Nothing '.') seconds
  pure result

-- * error handling

throwWithSeverity :: (HasCallStack) => Severity -> Error -> M a
throwWithSeverity severity e = do
  spans <- asks spanCtx
  throwError $ ErrorWithContext {callstack = callStack, spans, severity, err = e}

throw :: (HasCallStack) => Error -> M a
throw = throwWithSeverity Error

shortcut :: (HasCallStack) => Error -> M a
shortcut = throwWithSeverity Informational

rethrow :: ErrorWithContext -> M a
rethrow = throwError

catchIfErrorMatches :: M a -> IndexedTraversal' Int T.Text RE.Match -> (NonEmpty [Text] -> M a) -> M a
catchIfErrorMatches action regex fallback = do
  action
    `catchError` ( \e -> case err e of
                     RunProcessError {..} -> case stdErr ^.. regex . RE.groups of
                       [] -> throwError e
                       m : rest -> fallback (m :| rest)
                     _ -> throwError e
                 )

catchEither :: M a -> (Either SomeException ErrorWithContext -> M a) -> M a
catchEither action handler = (action `catchError` (handler . Right)) `catchAny` (handler . Left)

rethrowEither :: Either SomeException ErrorWithContext -> M a
rethrowEither = \case
  Right e -> rethrow e
  Left e -> SafeException.throwIO e

tryEither :: M a -> M (Either (Either SomeException ErrorWithContext) a)
tryEither action = catchEither (Right <$> action) (pure . Left)

-- | Executes a given clean-up function on exceptions and errors.
-- Runs the clean-up functions also for async exceptions.
whenErrorEither :: M a -> (Either SomeException ErrorWithContext -> M ()) -> M a
whenErrorEither action onError =
  ( action
      `SafeException.withException` (onError . Left)
  )
    `catchError` ( \e -> do
                     onError $ Right e
                     throwError e
                 )

-- Catches both IO and MonadError errors, logs them at Critical, but otherwise
-- ignores them
ignoringAllErrors :: M a -> M ()
ignoringAllErrors action =
  (void action `catchError` logIt) `SafeException.catchAny` logIt
  where
    logIt x = log Critical $ "Got an error (ignored): " <> show x

aesonDecode :: (HasCallStack) => Text -> (Value -> Parser a) -> Text -> M a
aesonDecode description parser json = do
  case eitherDecode' (cs json) >>= Data.Aeson.Types.parseEither parser of
    Left e ->
      throw
        $ DecodeError
          { original = json,
            message =
              "Could not decode "
                <> description
                <> ". Error was: "
                <> cs e
          }
    Right parsed -> pure parsed

safeGetAbsoluteFlakeDir :: FlakeDir -> M FilePath
safeGetAbsoluteFlakeDir (FlakeDir flakeDir) = do
  curDir <- view #workingDir
  result <- liftIO $ canonicalizePath (curDir </> flakeDir)
  if result == curDir || (curDir <> "/") `isPrefixOf` result
    then pure result
    else do
      throw $ OtherError $ "'" <> cs flakeDir <> "' is not a path within the repo"
