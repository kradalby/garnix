module Garnix.Hosting.Deploy
  ( rolloutNewServerVersion,
    startServer,
    stopUnusedServers,
    stopServer,
    redeployServer,
    -- Exported for tests
    _costBreakdown,
    DeployCounts (..),
    checkDeployPlan,
  )
where

import Control.Concurrent.Async.Lifted qualified as Async
import Control.Lens (traversed)
import Cradle
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Garnix.API.Keys (getRepoKeys)
import Garnix.BuildLogs.Types (mkLogLine)
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Entitlements qualified as Entitlements
import Garnix.Hosting.ServerPool qualified as ServerPool
import Garnix.Hosting.ServerPool.Types
import Garnix.Monad
import Garnix.Monad.Polling (PollingConfig (PollingConfig), withPolling)
import Garnix.Monad.SubProcess (runSubProcess)
import Garnix.MonetaryCost
import Garnix.Nix.StorePath (withStorePath)
import Garnix.Nix.Types
import Garnix.Prelude
import Garnix.Reporters.Utils (withRunReporter)
import Garnix.Request
import Garnix.Types
import Garnix.YamlConfig

-- | Deploys new server versions, deletes old ones.
rolloutNewServerVersion ::
  Reporter ->
  CommitInfo ->
  DeploymentType ->
  M [ServerInfo]
rolloutNewServerVersion reporter commitInfo deploymentType =
  withTextSpan ("deployment_type", fromDeploymentType (const "branch") (const "pr") deploymentType)
    $ (<?> "Rolling out new servers")
    $ do
      plan <- getDeployPlan reporter commitInfo deploymentType
      executeDeployPlan reporter commitInfo plan deploymentType

stopUnusedServers :: M ()
stopUnusedServers = do
  (PrHostList runningServers) <- DB.getShutdownCandidates
  heartbeat <- DB.getRecentHeartbeats
  let toSpinDown = filter (haveNotSentHeartbeat heartbeat) runningServers
  traverse_ (\s -> stopServer (s ^. serverId) (s ^. hetznerId)) toSpinDown
  where
    haveNotSentHeartbeat :: [Text] -> Host -> Bool
    haveNotSentHeartbeat heartbeats host =
      let hostName = hostToDomainName host <> ".garnix.me"
       in hostName `notElem` heartbeats

getDeployPlan ::
  Reporter ->
  CommitInfo ->
  DeploymentType ->
  M DeployPlan
getDeployPlan reporter commitInfo deploymentType = do
  withErrorReporter reporter commitInfo $ do
    cfg <- getConfig
    let wantedPackagesMapping :: Map PackageName (ServerTier, Bool) = Map.fromList $ case deploymentType of
          BranchDeployment thisBranch -> flip mapMaybe (cfg ^. serverSection)
            $ \s -> case s ^. deploySection of
              OnBranch branch serverTier isPrimary | branch == thisBranch -> Just (s ^. configuration, (serverTier, isPrimary))
              _ -> Nothing
          GhPrDeployment _prId ->
            (cfg ^. serverSection)
              & filter (\s -> s ^. deploySection == OnPullRequest)
              & map (\s -> (s ^. configuration, (def, False)))
    let wantedPackages = Map.keys wantedPackagesMapping
    existing <- DB.getRunningServersOf (commitInfo ^. repoInfo) deploymentType
    wantedBuilds <- withPolling (PollingConfig (fromSeconds @Int 2) (fromHours @Int 2)) $ do
      builds <- DB.getLatestBuildsMatching (commitInfo ^. repoInfo) (commitInfo ^. commit)
      let overallPackageFinished = case find (\build -> build ^. packageType == TypeOverall) builds of
            Nothing -> False
            Just b -> isJust (b ^. status)
          wantedAndStarted =
            filter
              ( \build ->
                  build
                    ^. package
                    `elem` wantedPackages
                    && build
                    ^. packageType
                    == TypeNixosConfiguration
              )
              builds
          wantedAndFinished = filter (\build -> isJust (build ^. status)) wantedAndStarted
          wantedAndNotStarted = filter (`notElem` wantedAndStarted ^.. each . package) wantedPackages
          uploadedAllBuilds = all (\b -> b ^. uploadedToCache == Just True) wantedAndFinished
      when (overallPackageFinished && not (null wantedAndNotStarted)) $ do
        throw $ DeploymentWantsNixosConfigurationsThatDontExist wantedAndNotStarted
      pure
        $ if sort wantedPackages == sort (wantedAndFinished ^.. each . package) && uploadedAllBuilds
          then Just wantedAndFinished
          else Nothing

    let toRedeploy =
          [ (server, build)
            | server <- existing,
              build <- wantedBuilds,
              server ^. buildPersistenceName == build ^. persistenceName,
              isJust (build ^. persistenceName)
          ]
        toSpinDown = filter (`notElem` (fst <$> toRedeploy)) existing
    toSpinUp <-
      wantedBuilds
        & filter (`notElem` (snd <$> toRedeploy))
        & mapM
          ( \build -> case Map.lookup (build ^. package) wantedPackagesMapping of
              Just (serverTier, domainIsPrimary) -> pure $ ServerToSpinUp {serverTier, build, domainIsPrimary}
              Nothing -> throw $ OtherError "impossible: wantedPackagesMap should contain all deployable packages"
          )
    let plan = DeployPlan toSpinDown toSpinUp toRedeploy
    unless (null toSpinUp) $ do
      checkDeployPlan (commitInfo ^. repoInfo) deploymentType plan
    pure plan

-- | Will catch all exceptions, create a new reporter to report the error to the user and rethrow.
withErrorReporter :: Reporter -> CommitInfo -> M a -> M a
withErrorReporter reporter commitInfo action = do
  checkResult <- try action
  case checkResult of
    Left error -> do
      run <- DB.newRun "deployment plan" commitInfo
      runReporter <- createNewRun reporter (ReportRun run)
      reportLogs runReporter (mkLogLine $ userMessage $ toErrorDetails error)
      reportComplete runReporter RunReportStatusFailure
      rethrow error
    Right a -> pure a

checkDeployPlan ::
  RepoInfo ->
  DeploymentType ->
  DeployPlan ->
  M ()
checkDeployPlan repoInfo deploymentType plan = do
  checkEntitlement (ghPrDeployment deploymentType) plan (repoInfo ^. ghRepoOwner)
  checkServerTiers repoInfo plan
  checkSubdomainValidity repoInfo deploymentType $ fmap (^. #build) $ plan ^. #toSpinUp
  checkAllBuildsSucceeded plan

data DeployCounts = DeployCounts
  { includedInPlan :: Int64,
    notIncludedInPlan :: Int64
  }
  deriving (Generic, Show)

totalCount :: DeployCounts -> Int64
totalCount d = d ^. #includedInPlan + d ^. #notIncludedInPlan

costForTierDeployment :: ServerTier -> DeployCounts -> MonetaryCost
costForTierDeployment tier deployCounts = serverTierToCost tier `multiplyCost` (deployCounts ^. #notIncludedInPlan)

totalDeploymentCost :: Map ServerTier DeployCounts -> MonetaryCost
totalDeploymentCost =
  foldr' addCost (usd 0)
    . map (uncurry costForTierDeployment)
    . Map.toList

_costBreakdown :: Map ServerTier DeployCounts -> [Text]
_costBreakdown m =
  m
    & Map.toList
    & filter (\(_, d) -> totalCount d > 0)
    & map
      ( \(tier, deployCounts) ->
          let tierCostStr = formatCost (serverTierToCost tier)
           in serverTierToText tier
                <> " (x"
                <> show (totalCount deployCounts)
                <> ") = "
                <> formatCost (costForTierDeployment tier deployCounts)
                <> case deployCounts of
                  DeployCounts 0 1 -> ""
                  DeployCounts 0 _ -> " (" <> tierCostStr <> " each)"
                  DeployCounts n 0 -> " (" <> show n <> " included in plan)"
                  DeployCounts n 1 -> " (" <> show n <> " included in plan)"
                  DeployCounts inc notInc ->
                    " (" <> show inc <> " included in plan, " <> show notInc <> " not included at " <> tierCostStr <> " each)"
      )

checkEntitlement :: Maybe PullRequestId -> DeployPlan -> RepoOwner -> M ()
checkEntitlement mPrId plan repoOwner = do
  hostingLimits <- Entitlements.getHosting repoOwner
  case mPrId of
    Just _ -> do
      when (hostingLimits ^. #maxPrDeploymentTime == emptyDuration)
        $ throw
        $ EntitlementError
        $ "Sorry, hosting is not allowed for "
        <> getForgeLogin (getRepoOwner repoOwner)
        <> "."
      usedMinutes <- DB.getPrDeployDurationForOwner repoOwner
      when (usedMinutes > hostingLimits ^. #maxPrDeploymentTime)
        $ throw
        $ EntitlementError "Sorry, you have exhausted all your PR deployment minutes."
    Nothing -> do
      currentHosts <- DB.getRunningBranchServersForOwner repoOwner
      let allHostsAfterDeploy :: Map ServerTier DeployCounts =
            currentHosts
              & adjustCount succ (plan ^.. #toSpinUp . each . #serverTier)
              & adjustCount pred (plan ^.. #toSpinDown . each . tier)
              & Map.mapWithKey
                ( \tier count ->
                    let includedInPlan =
                          if tier == serverTierIncludedWithPlans
                            then min count (hostingLimits ^. #planIncludedBranchDeploymentHosts)
                            else 0
                     in DeployCounts
                          { includedInPlan,
                            notIncludedInPlan = count - includedInPlan
                          }
                )
      let costAfterDeploy = totalDeploymentCost allHostsAfterDeploy
      when (costAfterDeploy > hostingLimits ^. #extraBranchHostingSpend) $ do
        throw
          $ EntitlementError
          $ "Deploying this would result in "
          <> formatCost costAfterDeploy
          <> " extra monthly server cost above your plan. "
          <> ( if hostingLimits ^. #extraBranchHostingSpend > usd 0
                 then "However, you have configured a max spend of " <> formatCost (hostingLimits ^. #extraBranchHostingSpend) <> " per month on deployed hosts. "
                 else "You have not configured any additional spending budget on deployed hosts. "
             )
          <> "You can configure this spending limit on your account page at garnix.io.\n\n"
          <> "Here is a breakdown of what would be deployed by this commit and the associated costs:\n"
          <> T.unlines (map ("  " <>) $ _costBreakdown allHostsAfterDeploy)
  where
    adjustCount :: (Int64 -> Int64) -> [ServerTier] -> Map ServerTier Int64 -> Map ServerTier Int64
    adjustCount adjust servers map = foldr' (Map.alter (Just . adjust . fromMaybe 0)) map servers

checkServerTiers :: RepoInfo -> DeployPlan -> M ()
checkServerTiers repoInfo plan = do
  hostingLimits <- Entitlements.getHosting (repoInfo ^. ghRepoOwner)
  unless (hostingLimits ^. #largerServers) $ do
    forM_ (plan ^.. #toSpinUp . each . #serverTier) $ \serverTier ->
      when (serverTier /= def) $ do
        throw $ EntitlementError $ "Only server tier `" <> serverTierToText def <> "` supported."

checkSubdomainValidity ::
  RepoInfo ->
  DeploymentType ->
  [Build] ->
  M ()
checkSubdomainValidity
  (RepoInfo _ (RepoOwner (ForgeLogin repoOwner')) (RepoName repoName'))
  deploymentType
  wantedServers = do
    unless (isValidSubdomainString repoOwner')
      $ throw
      $ NameIsNotValidSubdomain RepoOwnerSubdomain repoOwner'
    unless (isValidSubdomainString repoName')
      $ throw
      $ NameIsNotValidSubdomain RepoNameSubdomain repoName'
    case deploymentType of
      BranchDeployment (Branch branch') -> do
        unless (isValidSubdomainString branch')
          $ throw
          $ NameIsNotValidSubdomain BranchSubdomain branch'
      GhPrDeployment _ -> pure ()
    forM_ wantedServers $ \build -> do
      let packageName = getPackageName $ build ^. package
      unless (isValidSubdomainString packageName)
        $ throw
        $ NameIsNotValidSubdomain PackageNameSubdomain packageName
      let persistence = build ^. persistenceName
      forM_ persistence $ \persistenceName -> do
        unless (isValidSubdomainString persistenceName)
          $ throw
          $ NameIsNotValidSubdomain PersistenceNameSubdomain persistenceName

checkAllBuildsSucceeded :: DeployPlan -> M ()
checkAllBuildsSucceeded plan = do
  forM_ (plan ^.. #toSpinUp . each . #build) $ \build -> do
    let packageName = getPackageName $ build ^. package
    case build ^. status of
      Just Success -> pure ()
      Just Failure -> throw $ OtherError $ packageName <> " failed"
      Just Timeout -> throw $ OtherError $ packageName <> " timed out"
      Just Cancelled -> throw $ OtherError $ packageName <> " cancelled"
      -- this should be unreachable since checkAllBuildsSucceeded is only called after waiting for all builds to finish
      Nothing -> throw $ OtherError $ packageName <> " has no status"

executeDeployPlan ::
  Reporter ->
  CommitInfo ->
  DeployPlan ->
  DeploymentType ->
  M [ServerInfo]
executeDeployPlan = curry4
  $ mockable #executeDeployPlanMock
  $ \(reporter, commitInfo, DeployPlan currentServers wantedServers redeployServers, deploymentType) -> do
    serverInfos <- Async.mapConcurrently (startServer reporter commitInfo deploymentType) wantedServers
    redeployedServers <- Async.mapConcurrently (uncurry (redeployServer reporter commitInfo deploymentType)) redeployServers
    deployedServerInfos <- toggleServerFlags serverInfos currentServers <?> "Toggling servers ready flags."
    Async.mapConcurrently_ (\s -> stopServer (s ^. id) (s ^. hetznerServerId)) currentServers
    return (deployedServerInfos <> redeployedServers)

stopServer :: ServerId -> HetznerServerId -> M ()
stopServer serverId hetznerId = do
  deleteServer hetznerId
  DB.deleteServerDB serverId

startServer ::
  Reporter ->
  CommitInfo ->
  DeploymentType ->
  ServerToSpinUp ->
  M ServerInfo
startServer = curry4
  $ mockable #startServerMock
  $ \(reporter, commitInfo, deploymentType, serverToSpinUp) -> do
    run <- DB.newRun ("deployment " <> getPackageName (serverToSpinUp ^. #build . package)) commitInfo
    withRunReporter reporter (ReportRun run) $ \runReporter -> do
      serverInfo <- ServerPool.createServer (commitInfo ^. repoInfo) deploymentType serverToSpinUp
      (serverInfo, stderr) <-
        setupServer (commitInfo ^. repoInfo) (serverToSpinUp ^. #build) serverInfo `whenError` \error -> do
          let logs = showPretty (err error)
          DB.appendToServerDeployLog (serverInfo ^. id) logs
      let logs =
            T.unlines
              [ "Server has been successfully deployed to: https://"
                  <> getPackageName (serverToSpinUp ^. #build . package)
                  <> "."
                  <> fromDeploymentType getBranch (("pull-" <>) . show . getPullRequestId) deploymentType
                  <> "."
                  <> getRepoName (commitInfo ^. repoInfo . ghRepoName)
                  <> "."
                  <> getForgeLogin (getRepoOwner (commitInfo ^. repoInfo . ghRepoOwner))
                  <> ".garnix.me",
                "ipv4: " <> serverInfo ^. ipv4Addr,
                "ipv6: " <> serverInfo ^. ipv6Addr,
                "",
                "logs:"
                  <> stderr
              ]
      DB.appendToServerDeployLog (serverInfo ^. id) logs
      reportLogs runReporter (mkLogLine logs)
      reportComplete runReporter RunReportStatusSuccess
      pure serverInfo

redeployServer :: Reporter -> CommitInfo -> DeploymentType -> ServerInfo -> Build -> M ServerInfo
redeployServer reporter commitInfo deploymentType serverInfo build = do
  withStorePath build "out" $ \case
    Nothing -> throw $ OtherError "Store path is missing"
    Just storePath -> do
      run <- DB.newRun ("redeployment " <> getPackageName (build ^. package)) commitInfo
      withRunReporter reporter (ReportRun run) $ \runReporter -> do
        copyClosure (SshUser "garnix") serverInfo storePath <?> "Copying closure for redeployment"
        deploymentLogs <- switchToConfiguration (SshUser "garnix") serverInfo storePath <?> "Switching to redeployment configuration"
        now <- liftIO getCurrentTime
        let serverInfo' =
              serverInfo
                & configurationBuildId
                .~ build
                ^. id
                & readyAt
                ?~ now
        DB.updateServerPostDeploy serverInfo' <?> "Updating DB about the redeployed server"
        let logs =
              T.unlines
                [ "Server has been successfully redeployed to: https://"
                    <> getPackageName (build ^. package)
                    <> "."
                    <> fromDeploymentType getBranch (("pull-" <>) . show . getPullRequestId) deploymentType
                    <> "."
                    <> getRepoName (commitInfo ^. repoInfo . ghRepoName)
                    <> "."
                    <> getForgeLogin (getRepoOwner (commitInfo ^. repoInfo . ghRepoOwner))
                    <> ".garnix.me",
                  "ipv4: " <> serverInfo ^. ipv4Addr,
                  "ipv6: " <> serverInfo ^. ipv6Addr,
                  "",
                  "logs:"
                    <> deploymentLogs
                ]
        reportLogs runReporter (mkLogLine logs)
        reportComplete runReporter RunReportStatusSuccess
        pure serverInfo'

-- | Toggles the wanted servers to be ready and the current servers to end.
toggleServerFlags :: [ServerInfo] -> [ServerInfo] -> M [ServerInfo]
toggleServerFlags wanted current = do
  currentTime <- liftIO getCurrentTime
  let wanted' = wanted & traversed . readyAt ?~ currentTime
      current' = current & traversed . endedAt ?~ currentTime
  DB.pgTransaction $ traverse_ DB.updateServerPostDeploy (wanted' <> current')
  pure wanted'

-- | Provisions a *new* server, and deploys the given NixOS configuration there.
--
-- Does *not* delete the old server.
setupServer ::
  RepoInfo ->
  Build ->
  ServerInfo ->
  M (ServerInfo, Text)
setupServer = curry3
  $ mockable #setupServerMock
  $ \(repoInfo, build, serverInfo) -> do
    withStorePath build "out" $ \case
      Nothing -> throw $ OtherError "Store path is missing"
      Just storePath -> do
        copyKeys repoInfo serverInfo
        copyClosure (SshUser "root") serverInfo storePath <?> "Copying closure"
        stderr <- switchToConfiguration (SshUser "root") serverInfo storePath <?> "Switching to configuration"
        return (serverInfo, stderr)

newtype SshUser = SshUser Text

copyClosure :: SshUser -> ServerInfo -> StorePath -> M ()
copyClosure (SshUser user) server storePath = do
  (ip, sshArgs) <- ServerPool.sshArgsFor server
  retryingFor (fromMinutes @Int 1) $ do
    runSubProcess
      $ cmd "nix-copy-closure"
      & addArgs ["--to", user <> "@" <> ip, cs storePath]
      & modifyEnvVar "NIX_SSHOPTS" (const $ Just $ cs $ T.intercalate " " sshArgs)

copyKeys :: RepoInfo -> ServerInfo -> M ()
copyKeys repoInfo server = do
  (ip, sshArgs) <- ServerPool.sshArgsFor server
  let keyLocation = "/var/garnix/keys/repo-key"
  let doRemotely cmd =
        runSubProcess
          $ Cradle.cmd "ssh"
          & addArgs (sshArgs <> ["root@" <> cs ip, cmd])
  doRemotely $ "mkdir -p " <> cs (takeDirectory keyLocation)
  (_, privKey) <-
    getRepoKeys (repoInfo ^. ghRepoOwner) (repoInfo ^. ghRepoName)
      <?> "Get private keys"
  repoSecretsKey <- view #repoSecretsEncryptionKeyPath
  exportResult <-
    liftIO
      ( exportKeys
          ExportKeysOpts
            { privateKey = privKey,
              ipAddr = ip,
              targetPath = keyLocation,
              sshArgs
            }
          repoSecretsKey
      )
      <?> "Export private keys to server"
  whenIs _Left exportResult $ throw . ProvisioningError
  doRemotely $ "chmod 400 " <> cs keyLocation

switchToConfiguration :: SshUser -> ServerInfo -> StorePath -> M Text
switchToConfiguration (SshUser user) server storePath = do
  (ip, sshArgs) <- ServerPool.sshArgsFor server
  StderrRaw stderr <-
    withError (errLens %~ toActivationError server)
      $ runSubProcess
      $ cmd "ssh"
      & addArgs
        ( sshArgs
            <> [ user <> "@" <> ip,
                 "sudo",
                 cs $ cs storePath </> "bin/switch-to-configuration",
                 "switch"
               ]
        )
      & silenceStdout
  pure $ cs stderr
  where
    toActivationError serverInfo = \case
      RunProcessError {stdErr} ->
        ActivationError serverInfo stdErr
      error -> error
