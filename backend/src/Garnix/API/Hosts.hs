module Garnix.API.Hosts
  ( getHostsForTraefik,
    postHostsHeartbeat,
    hostsAPI,
    HostsAPI,
    getHosts,
    HostList (..),
  )
where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Garnix.DB qualified as DB
import Garnix.GithubInterface.Types
import Garnix.Hosting.Deploy (stopServer)
import Garnix.Hosting.Helpers
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import Servant.Auth.Server

data HostsAPI route = HostsAPI
  { _hostsAPIGetHostsForTraefik :: route :- "traefik" :> Get '[JSON] HostList,
    _hostsAPIHeartbeat :: route :- "heartbeat" :> ReqBody '[JSON] [Text] :> Post '[JSON] NoContent,
    _hostsAPIGetIPsForDns :: route :- "dns" :> Get '[JSON] DnsHosts,
    _hostsAPIGetDomainsForOnDemandResolver :: route :- "on-demand-resolver" :> Get '[JSON] OnDemandResolverDomainNames,
    _hostsAPIGetHosts :: route :- Auth '[JWT, Cookie] AuthJwtPayload :> Get '[JSON] [RunningServer],
    _hostsAPIDeleteHost :: route :- Auth '[JWT, Cookie] AuthJwtPayload :> Capture "serverId" ServerId :> Delete '[JSON] ()
  }
  deriving (Generic)

hostsAPI :: HostsAPI (AsServerT M)
hostsAPI =
  HostsAPI
    { _hostsAPIGetHostsForTraefik = getHostsForTraefik,
      _hostsAPIHeartbeat = postHostsHeartbeat,
      _hostsAPIGetIPsForDns = getHostsForDns,
      _hostsAPIGetDomainsForOnDemandResolver = getDomainsForOnDemandResolver,
      _hostsAPIGetHosts = getHosts,
      _hostsAPIDeleteHost = deleteHost
    }

data HostList = HostList
  { hostList :: [Host],
    hostBaseUrl :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON HostList where
  toJSON (HostList hosts baseUrl) =
    let routerMapPair serviceDomain ruleDomain =
          ( ruleDomain,
            [aesonQQ| {
              service: #{serviceDomain},
              rule: #{"Host(`" <> ruleDomain <> ".garnix.me`)"},
              middlewares: ["heartbeatmiddleware"]
              }
            |]
          )

        httpRouters =
          Map.fromList
            $ concatMap
              ( \h ->
                  [routerMapPair (hostToDomainName h) (hostToDomainName h)]
                    <> if h ^. isPrimary then [routerMapPair (hostToDomainName h) (hostToPrimaryDomainName h)] else []
              )
              hosts
        httpService host =
          [aesonQQ|
            { loadBalancer:
                  { servers: [
                     { url: #{"http://" <> _hostIpV4Addr host}}
                    ]
                  }
            }
          |]
        httpServices = Map.fromList $ [(hostToDomainName h, httpService h) | h <- hosts]
     in [aesonQQ|
         {
          http:
             {
              routers: #{httpRouters},
              services: #{httpServices},
              middlewares: {
                heartbeatmiddleware: {
                  plugin: {
                    heartbeatmiddleware: {
                      reportEndpoint: #{baseUrl <> "/api/hosts/heartbeat"}
                    }
                  }
                }
              }
             }
         }
       |]

getHostsForTraefik :: M HostList
getHostsForTraefik = do
  baseUrl <- view #baseUrl
  hosts <-
    DB.getAllRunningHosts
      <&> filter
        ( \host ->
            isValidSubdomainString (host ^. repoOwner . to getRepoOwner . to getForgeLogin)
              && isValidSubdomainString (host ^. repoName . to getRepoName)
              && (isValidSubdomainString (host ^. branch . to getBranch) || isJust (host ^. pullRequest))
              && isValidSubdomainString (host ^. packageName . to getPackageName)
        )
  pure $ HostList hosts baseUrl

postHostsHeartbeat :: [Text] -> M NoContent
postHostsHeartbeat hosts = NoContent <$ DB.upsertHeartbeat hosts

data DnsHosts = DnsHosts
  { byHash :: Map Text HostIPs,
    byName :: Map Text HostIPs
  }
  deriving (Generic, ToJSON)

data HostIPs = HostIPs {ipv4 :: Text, ipv6 :: Text}
  deriving (Eq, Show, Generic, ToJSON)

getHostsForDns :: M DnsHosts
getHostsForDns = do
  runningHosts <- DB.getAllRunningHosts
  let mapRunningHosts :: (Host -> Maybe Text) -> Map Text HostIPs
      mapRunningHosts getName =
        Map.fromList
          $ mapMaybe
            ( \host -> do
                name <- getName host
                pure
                  ( name,
                    HostIPs
                      { ipv4 = host ^. ipV4Addr,
                        ipv6 = host ^. ipV6Addr
                      }
                  )
            )
            runningHosts
  let byHash = mapRunningHosts $ \host -> do
        drvPath <- host ^. drvPath
        T.take 32 <$> T.stripPrefix "/nix/store/" (cs drvPath)
  let byName = mapRunningHosts $ Just . hostToDomainName
  pure $ DnsHosts {byHash, byName}

getHosts :: AuthResult AuthJwtPayload -> M [RunningServer]
getHosts (Authenticated (WebSession user ghToken)) = do
  getRunningAndRecentServersForOwners
    . (RepoOwner (user ^. githubLogin) :)
    . map organizationName
    =<< getInstalledOrgs ghToken
getHosts _ = throw Unauthorized

deleteHost :: AuthResult AuthJwtPayload -> ServerId -> M ()
deleteHost (Authenticated (WebSession user ghToken)) serverId = do
  orgs <-
    (RepoOwner (user ^. githubLogin) :)
      . map organizationName
      <$> getInstalledOrgs ghToken
  hetznerServerIds <- do
    DB.getHetznerServerById orgs serverId >>= \case
      Nothing -> pure []
      Just serverId -> do
        pure [serverId]
  case hetznerServerIds of
    [hetznerServerId] -> do stopServer serverId hetznerServerId
    _ -> throw NotFound
deleteHost _ _ = throw Unauthorized

data OnDemandResolverDomainNames = OnDemandResolverDomainNames
  { domains :: [Text]
  }
  deriving (Generic, ToJSON)

getDomainsForOnDemandResolver :: M OnDemandResolverDomainNames
getDomainsForOnDemandResolver = do
  runningHosts <- DB.getAllRunningHosts
  pure
    $ OnDemandResolverDomainNames
      { domains =
          concatMap
            ( \host ->
                [hostToDomainName host <> ".garnix.me"]
                  <> if host ^. isPrimary then [hostToPrimaryDomainName host <> ".garnix.me"] else []
            )
            runningHosts
      }

hostToPrimaryDomainName :: Host -> Text
hostToPrimaryDomainName host =
  getRepoName (_hostRepoName host)
    <> "."
    <> getForgeLogin (getRepoOwner (_hostRepoOwner host))
