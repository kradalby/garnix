module Garnix.HetznerInterface
  ( realHetznerInterface,
    _parseCreateServerResponse,
  )
where

import Control.Lens (_Show)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Integer, _String)
import Data.Text.Lens (unpacked)
import Garnix.Hosting.ServerPool.Types
import Garnix.Monad
import Garnix.Prelude hiding (get, put)
import Garnix.Types
import Network.HTTP.Client (Response)
import Network.Wreq qualified as Wreq

realHetznerInterface :: HetznerInterface
realHetznerInterface =
  HetznerInterface
    { _hetznerInterfaceProvisionServer = provisionServer',
      _hetznerInterfaceUpdateMetadata = updateMetadata',
      _hetznerInterfaceDeleteServer = deleteServer',
      _hetznerInterfaceGetServerStatus = getServerStatus'
    }

-- | Provisions a new server, returning the server info. The server may still
-- be in "initializing" state.
provisionServer' :: PreprovisionedServerId -> HetznerLocation -> HetznerServerType -> M PreprovisionedServer
provisionServer' (PreprovisionedServerId serverId) serverLocation serverType = do
  let json =
        [aesonQQ|
          {
            "name": #{show serverId},
            "server_type": #{hetznerServerTypeToName serverType},
            "location": #{hetznerLocationToName serverLocation},
            "start_after_create": true,
            "image": "ubuntu-22.04",
            "labels": {},
            "ssh_keys": [
              "garnix_server_ssh_hosting_pub"
            ],
            "user_data": "#cloud-config\nruncmd:\n- curl https://raw.githubusercontent.com/jfroche/nixos-infect/403911527f0eccabacf00a7924aa6208ab98c3b4/nixos-infect | NIX_INSTALL_URL=https://releases.nixos.org/nix/nix-2.19.3/install PROVIDER=hetznercloud NIX_CHANNEL=nixos-24.11 bash 2>&1 | tee /tmp/infect.log\n",
            "automount": false
          }
        |]
  response <- post "https://api.hetzner.cloud/v1/servers" json
  let code = response ^. Wreq.responseStatus . Wreq.statusCode
  if 200 <= code && code < 300
    then do
      now <- liftIO getCurrentTime
      case _parseCreateServerResponse now (response ^. Wreq.responseBody) of
        Nothing ->
          throw
            $ ProvisioningError
              { message =
                  "Couldn't parse Hetzner server provisioning response. "
                    <> "(Note that the server may need to be deleted!) "
                    <> "Response: "
                    <> show response
              }
        Just serverInfo -> pure serverInfo
    else throw $ ProvisioningError {message = cs $ show response}

updateMetadata' ::
  RepoInfo ->
  DeploymentType ->
  Build ->
  ServerId ->
  HetznerServerId ->
  M ()
updateMetadata' repoInfo deploymentType build (ServerId serverId) (HetznerServerId hetznerId) = do
  let (branchOrPr, branchOrPrValue) = case deploymentType of
        BranchDeployment (Branch branch) -> ("branch", branch)
        GhPrDeployment (PullRequestId prId) -> ("pr", show prId)
  let json =
        [aesonQQ|
          {
            "name": #{serverId ^. re hashIdText},
            "labels": {
              "repo_org": #{repoInfo ^. ghRepoOwner},
              "repo_name": #{repoInfo ^. ghRepoName},
              $branchOrPr: #{branchOrPrValue},
              "nixosConfiguration": #{getPackageName $ build ^. package},
              "commit": #{build ^. gitCommit}
              }
            }
          |]
  response <- put (cs $ "https://api.hetzner.cloud/v1/servers/" <> show hetznerId) json
  let code = response ^. Wreq.responseStatus . Wreq.statusCode
  if 200 <= code && code < 300
    then pure ()
    else do
      throw
        $ ProvisioningError
          { message = "Non-2XX response from Hetzner when updating server metadata. Response: " <> show response
          }

deleteServer' :: HetznerServerId -> M ()
deleteServer' serverId = do
  let sId = show (coerce serverId :: Int32)
  resp <- delete . cs $ "https://api.hetzner.cloud/v1/servers/" <> sId
  case resp ^? Wreq.responseBody . key "action" . key "status" of
    Just "error" -> do
      log Error $ "Hetzner deletion failed with: " <> show (resp ^. Wreq.responseBody)
      throw $ ProvisioningError $ "Could not delete server. Response: " <> show resp
    _ -> pure ()

getServerStatus' :: HetznerServerId -> M Text
getServerStatus' hetznerId = do
  response <-
    get
      . cs
      $ "https://api.hetzner.cloud/v1/servers/"
      <> show (coerce hetznerId :: Int32)
  let code = response ^. Wreq.responseStatus . Wreq.statusCode
  if 200 <= code && code < 300
    then
      let status' =
            response
              ^? Wreq.responseBody
              . key "server"
              . key "status"
              . _String
       in case status' of
            Just st -> pure st
            Nothing -> throw $ ProvisioningError $ "getServerStatus': Missing 'status' field. Response: " <> show response
    else throw $ ProvisioningError $ "getServerStatus': non-2XX status code. Response: " <> show response

-- * Response parsing

_parseCreateServerResponse :: UTCTime -> LazyByteString -> Maybe PreprovisionedServer
_parseCreateServerResponse created bs = bs ^? key "server" >>= parsePreprovisionedServer created

parsePreprovisionedServer :: UTCTime -> Aeson.Value -> Maybe PreprovisionedServer
parsePreprovisionedServer created v = do
  serverId <- v ^? key "name" . _String . unpacked . _Show
  hetznerId <- HetznerServerId . fromIntegral <$> v ^? key "id" . _Integer
  ip4 <- v ^? key "public_net" . key "ipv4" . key "ip" . _String
  ip6 <- v ^? key "public_net" . key "ipv6" . key "ip" . _String
  pure
    $ PreprovisionedServer
      { _preprovisionedServerId = PreprovisionedServerId serverId,
        _preprovisionedServerHetznerServerId = hetznerId,
        _preprovisionedServerIpv4Addr = ip4,
        _preprovisionedServerIpv6Addr = ip6,
        _preprovisionedServerCreatedAt = created,
        _preprovisionedServerReadyAt = Nothing
      }

-- We want to use *our* created time, not hetzner's.

-- * Hetzner request helpers

put :: String -> Aeson.Value -> M (Response LazyByteString)
put req body = do
  opts <- wreqOptions
  liftIO (Wreq.putWith opts req body) `catch` \(e :: SomeException) ->
    throw $ ProvisioningError $ "Error from PUT request: " <> show e

post :: String -> Aeson.Value -> M (Response LazyByteString)
post req body = do
  opts <- wreqOptions
  liftIO (Wreq.postWith opts req body) `catch` \(e :: SomeException) ->
    throw $ ProvisioningError $ "Error from POST request: " <> show e

get :: String -> M (Response LazyByteString)
get req = do
  opts <- wreqOptions
  liftIO (Wreq.getWith opts req) `catch` \(e :: SomeException) ->
    throw $ ProvisioningError $ "Error from GET request: " <> show e

delete :: String -> M (Response LazyByteString)
delete req = do
  opts <- wreqOptions
  liftIO (Wreq.deleteWith opts req) `catch` \(e :: SomeException) ->
    throw $ ProvisioningError $ "Error from DELETE request: " <> show e

wreqOptions :: M Wreq.Options
wreqOptions = do
  token <- view #hetznerToken
  mgr <- view #manager
  pure
    $ Wreq.defaults
    & Wreq.manager
    .~ Right mgr
    & Wreq.auth
    ?~ Wreq.oauth2Bearer token
    & Wreq.checkResponse
    ?~ \_ _ -> pure ()
