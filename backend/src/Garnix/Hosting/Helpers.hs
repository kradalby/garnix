module Garnix.Hosting.Helpers
  ( getRunningAndRecentServersForOwners,
    getBranchDeploymentBillingLineItems,
    calculateBranchDeploymentBillingLineItems,
    groupIdentifierToLineItemDescription,
    BranchServerGroupIdentifier (..),
    BranchServerBillingLineItem (..),
    RunningServer (..),
  )
where

import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Hosting.ServerPool.Types
import Garnix.Monad
import Garnix.MonetaryCost
import Garnix.Prelude
import Garnix.Types

data ServerStatus
  = Online
  | Failed
  | Booting
  | Ended
  deriving stock (Eq, Show, Generic)

instance ToJSON ServerStatus where
  toJSON = ourToJSON

data RunningServer = RunningServer
  { _runningServerId :: ServerId,
    _runningServerType :: DeploymentType,
    _runningServerStatus :: ServerStatus,
    _runningServerRepoOwner :: RepoOwner,
    _runningServerRepoName :: RepoName,
    _runningServerPackageName :: PackageName,
    _runningServerCreatedAt :: Maybe UTCTime,
    _runningServerConfigurationBuildId :: BuildId,
    _runningServerCommit :: CommitHash,
    _runningServerIpv4 :: Maybe Text,
    _runningServerDeployLogs :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RunningServer where
  toJSON = ourToJSON

getRunningAndRecentServersForOwners :: [RepoOwner] -> M [RunningServer]
getRunningAndRecentServersForOwners owners = do
  mapMaybe
    ( \(id, pr, branch, readyAt, endedAt, repoUser, repoName, packageName, createdAt, buildId, commit, ipv4, logs) -> do
        typ <- serverDeploymentType pr branch
        status <- serverStatus readyAt endedAt
        pure $ RunningServer id typ status repoUser repoName packageName createdAt buildId commit ipv4 logs
    )
    <$> DB.pgQuery
      [pgSQL|
        SELECT
          servers.id,
          servers.pull_request,
          builds.branch,
          servers.ready_at,
          servers.ended_at,
          builds.repo_user,
          builds.repo_name,
          builds.package,
          servers.created_at,
          servers.configuration_build_id,
          builds.git_commit,
          servers.ipv4,
          servers.deploy_logs
        FROM servers
        INNER JOIN builds
        ON servers.configuration_build_id = builds.id
        WHERE builds.repo_user = ANY(${owners})
        AND (servers.ended_at IS NULL OR servers.ended_at > now() - interval '24' hour)
        ORDER BY servers.created_at DESC
      |]
  where
    serverDeploymentType :: Maybe PullRequestId -> Maybe Branch -> Maybe DeploymentType
    serverDeploymentType pr branch = case (pr, branch) of
      (Just prId, _) -> Just $ GhPrDeployment prId
      (Nothing, Just branch) -> Just $ BranchDeployment branch
      (Nothing, Nothing) -> Nothing
    serverStatus :: Maybe UTCTime -> Maybe UTCTime -> Maybe ServerStatus
    serverStatus readyAt endedAt = case (readyAt, endedAt) of
      (Nothing, Nothing) -> Just Booting
      (Just _, Nothing) -> Just Online
      (Just _, Just _) -> Just Ended
      (Nothing, Just _) -> Nothing

data BranchServerGroupIdentifier = BranchServerGroupIdentifier
  { owner :: RepoOwner,
    repo :: RepoName,
    package :: PackageName,
    serverTier :: ServerTier
  }
  deriving stock (Eq, Ord, Show, Generic)

groupIdentifierToLineItemDescription :: BranchServerGroupIdentifier -> Text
groupIdentifierToLineItemDescription group =
  getForgeLogin (getRepoOwner $ group ^. #owner)
    <> "/"
    <> getRepoName (group ^. #repo)
    <> "#"
    <> getPackageName (group ^. #package)
    <> " "
    <> serverTierToText (group ^. #serverTier)

data BranchServerBillingLineItem = BranchServerBillingLineItem
  { group :: BranchServerGroupIdentifier,
    includedInPlan :: Bool,
    usedTime :: Duration,
    cost :: MonetaryCost
  }
  deriving stock (Eq, Ord, Show, Generic)

getBranchDeploymentBillingLineItems :: Int64 -> UTCTime -> UTCTime -> RepoOwner -> M [BranchServerBillingLineItem]
getBranchDeploymentBillingLineItems numFreeServers periodStart periodEnd owner = do
  now <- liftIO getCurrentTime
  servers <-
    -- Note: the `ready_at` and `ended_at` in the WHERE clause is only for
    -- limiting the returned results, billing period calculation actually
    -- happens in haskell in the map below.
    DB.pgQuery
      [pgSQL|
          SELECT
            ready_at,
            ended_at,
            server_tier,
            builds.repo_user,
            builds.repo_name,
            builds.package
          FROM servers
          INNER JOIN builds
          ON servers.configuration_build_id = builds.id
          WHERE builds.repo_user = ${owner}
          AND ready_at <= ${periodEnd}
          AND (ended_at IS NULL OR ended_at >= ${periodStart})
        |]
  calculateBranchDeploymentBillingLineItems numFreeServers periodStart periodEnd
    <$> mapM
      ( \(readyAt :: Maybe UTCTime, endTime :: Maybe UTCTime, serverTier :: ServerTier, repoOwner :: RepoOwner, repoName :: RepoName, package :: PackageName) -> do
          startTime <- case readyAt of
            Just s -> pure s
            Nothing -> throw $ OtherError "getBranchDeploymentBillingLineItems: Impossible: readyAt is null"
          let normalizedStart = max periodStart startTime
          let normalizedEnd = min periodEnd $ fromMaybe now endTime
          let group = BranchServerGroupIdentifier repoOwner repoName package serverTier
          let usedTime = normalizedEnd `diffTime` normalizedStart
          pure (group, usedTime)
      )
      servers

calculateBranchDeploymentBillingLineItems :: Int64 -> UTCTime -> UTCTime -> [(BranchServerGroupIdentifier, Duration)] -> [BranchServerBillingLineItem]
calculateBranchDeploymentBillingLineItems numFreeServers periodStart periodEnd usedTimeByGroup =
  let totalFreeTime = periodDuration `multiplyDuration` numFreeServers
   in fst
        $ foldl'
          ( \(lineItems, freeTimeRemaining) (group, usedTime) ->
              let (lineItemsToAdd, newFreeTimeRemaining) = generateLineItems freeTimeRemaining group usedTime
               in (lineItems ++ lineItemsToAdd, newFreeTimeRemaining)
          )
          ([], totalFreeTime)
        $ sortBy (compare `on` \(group, duration) -> (Down duration, group))
        $ Map.toList
        $ Map.fromListWith addDuration
        $ filter
          ((> emptyDuration) . snd)
          usedTimeByGroup
  where
    periodDuration :: Duration
    periodDuration = periodEnd `diffTime` periodStart

    -- Duration returned here is the updated free time remaining
    generateLineItems :: Duration -> BranchServerGroupIdentifier -> Duration -> ([BranchServerBillingLineItem], Duration)
    generateLineItems freeTimeRemaining group usedTime
      | not serverIsEligibleForFreeTier || freeTimeRemaining == emptyDuration =
          -- There is no free time remaining, or the server is not free. Bill this server for the entire usage
          ( [ BranchServerBillingLineItem
                { group = group,
                  includedInPlan = False,
                  usedTime = usedTime,
                  cost = serverCost `multiplyCost` (usedTime `divideDuration` periodDuration)
                }
            ],
            freeTimeRemaining
          )
      | freeTimeRemaining >= usedTime =
          -- We have still enough free time to cover this entire server use
          ( [ BranchServerBillingLineItem
                { group = group,
                  includedInPlan = True,
                  usedTime = usedTime,
                  cost = usd 0
                }
            ],
            freeTimeRemaining `subtractDuration` usedTime
          )
      | otherwise =
          -- This server is partly covered by free time, so we split it into two line items
          ( [ BranchServerBillingLineItem
                { group = group,
                  includedInPlan = True,
                  usedTime = freeTimeRemaining,
                  cost = usd 0
                },
              let unFreeUsedTime = usedTime `subtractDuration` freeTimeRemaining
               in BranchServerBillingLineItem
                    { group = group,
                      includedInPlan = False,
                      usedTime = unFreeUsedTime,
                      cost = serverCost `multiplyCost` (unFreeUsedTime `divideDuration` periodDuration)
                    }
            ],
            emptyDuration
          )
      where
        serverIsEligibleForFreeTier :: Bool
        serverIsEligibleForFreeTier = group ^. #serverTier == serverTierIncludedWithPlans

        serverCost :: MonetaryCost
        serverCost = serverTierToCost $ group ^. #serverTier
