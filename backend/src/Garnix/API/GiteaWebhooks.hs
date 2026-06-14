-- | Gitea\/Forgejo webhook ingestion.
--
-- Gitea posts a JSON body with an @X-Gitea-Event@ header and an
-- @X-Gitea-Signature@ header (hex HMAC-SHA256 of the raw body with the configured
-- webhook secret). We verify the signature, parse the (Gitea-flavoured) JSON into a
-- forge-neutral 'ForgeEvent', and hand it to the shared 'dispatchForgeEvent'.
module Garnix.API.GiteaWebhooks (GiteaWebhookAPI (..), giteaWebhookAPI) where

import Control.Lens (Fold)
import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Aeson.Lens (key, _Bool, _Integral, _String)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Garnix.Async (Promise)
import Garnix.Forge.Gitea (giteaRepoInfo)
import Garnix.Forge.Types (ForgeConfig (..), ForgeKind (..))
import Garnix.Monad
import Garnix.Monad.Async (emptyPromise, logPromiseErrors)
import Garnix.Orchestrator
import Garnix.Prelude
import Garnix.Types
import Servant

data GiteaWebhookAPI route = GiteaWebhookAPI
  { _giteaWebhook ::
      route
        :- Header "X-Gitea-Event" Text
        :> Header "X-Gitea-Signature" Text
        :> ReqBody '[OctetStream] BSL.ByteString
        :> Post '[JSON] ()
  }
  deriving (Generic)

giteaWebhookAPI :: GiteaWebhookAPI (AsServerT M)
giteaWebhookAPI = GiteaWebhookAPI {_giteaWebhook = handleGiteaWebhook}

handleGiteaWebhook :: (HasCallStack) => Maybe Text -> Maybe Text -> BSL.ByteString -> M ()
handleGiteaWebhook mEvent mSig body = do
  verifyGiteaSignature mSig body
  uniqueId <- randomBase64 64
  withTextSpan ("event_id", uniqueId) $ withTextSpan ("tag", "gitea webhook event") $ do
    case mEvent of
      Just "push" -> dispatchPush body >>= logPromiseErrors
      Just "pull_request" -> dispatchPullRequest body >>= logPromiseErrors
      other -> log Informational $ "Ignoring Gitea event: " <> show other

-- | Verify the @X-Gitea-Signature@ HMAC-SHA256 over the raw body.
verifyGiteaSignature :: (HasCallStack) => Maybe Text -> BSL.ByteString -> M ()
verifyGiteaSignature mSig body = do
  cfg <- forgeConfig Gitea
  let digest = hmacGetDigest (hmac (forgeConfigWebhookSecret cfg) (BSL.toStrict body) :: HMAC SHA256) :: Digest SHA256
      expected = cs (show digest) :: Text
  unless (Just expected == mSig)
    $ throw
    $ ForbiddenWithMessage "Invalid Gitea webhook signature"

dispatchPush :: (HasCallStack) => BSL.ByteString -> M (Promise ())
dispatchPush body = do
  (owner, repo) <- parseRepo body
  sha <- require body (key "after" . _String) "push.after"
  ref <- require body (key "ref" . _String) "push.ref"
  branch <- case T.stripPrefix "refs/heads/" ref of
    Just b -> pure (Branch b)
    Nothing -> throw $ OtherError $ "Gitea push to non-branch ref: " <> ref
  tok <- giteaApiToken
  let commitInfo =
        CommitInfo
          { _commitInfoReqUser = ForgeLogin (senderLogin body),
            _commitInfoRepoPublicity = RepoIsPublic (not (repoPrivate body)),
            _commitInfoRepoInfo = giteaRepoInfo tok owner repo,
            _commitInfoBranch = Just branch,
            _commitInfoPrFromFork = Nothing,
            _commitInfoCommit = CommitHash sha
          }
  dispatchForgeEvent (CommitBuild False commitInfo)

dispatchPullRequest :: (HasCallStack) => BSL.ByteString -> M (Promise ())
dispatchPullRequest body = do
  action <- require body (key "action" . _String) "pull_request.action"
  if action `notElem` ["opened", "synchronized", "reopened"]
    then emptyPromise
    else do
      (baseOwner, baseRepo) <- parseFullName =<< require body (key "repository" . key "full_name" . _String) "repository.full_name"
      headFull <- require body (key "pull_request" . key "head" . key "repo" . key "full_name" . _String) "pull_request.head.repo.full_name"
      baseFull <- require body (key "pull_request" . key "base" . key "repo" . key "full_name" . _String) "pull_request.base.repo.full_name"
      sha <- require body (key "pull_request" . key "head" . key "sha" . _String) "pull_request.head.sha"
      number <- require body (key "pull_request" . key "number" . _Integral) "pull_request.number"
      tok <- giteaApiToken
      let prFromFork = if headFull /= baseFull then Just (PrFromFork headFull) else Nothing
          commitInfo =
            CommitInfo
              { _commitInfoReqUser = ForgeLogin (senderLogin body),
                _commitInfoRepoPublicity = RepoIsPublic (not (repoPrivate body)),
                _commitInfoRepoInfo = giteaRepoInfo tok baseOwner baseRepo,
                _commitInfoBranch = Nothing,
                _commitInfoPrFromFork = prFromFork,
                _commitInfoCommit = CommitHash sha
              }
      dispatchForgeEvent (PullRequestBuild commitInfo (PullRequestId number))

-- * Parsing helpers

giteaApiToken :: (HasCallStack) => M ForgeToken
giteaApiToken = do
  cfg <- forgeConfig Gitea
  case forgeConfigApiToken cfg of
    Just tok -> pure tok
    Nothing -> throw $ OtherError "Gitea is configured without an API token"

parseRepo :: (HasCallStack) => BSL.ByteString -> M (RepoOwner, RepoName)
parseRepo body =
  parseFullName =<< require body (key "repository" . key "full_name" . _String) "repository.full_name"

parseFullName :: (HasCallStack) => Text -> M (RepoOwner, RepoName)
parseFullName fullName = case T.splitOn "/" fullName of
  [o, r] -> pure (RepoOwner (ForgeLogin o), RepoName r)
  _ -> throw $ OtherError $ "Unexpected Gitea repository full_name: " <> fullName

senderLogin :: BSL.ByteString -> Text
senderLogin body = fromMaybe "" (body ^? key "sender" . key "login" . _String)

repoPrivate :: BSL.ByteString -> Bool
repoPrivate body = fromMaybe False (body ^? key "repository" . key "private" . _Bool)

require :: (HasCallStack) => BSL.ByteString -> Fold BSL.ByteString a -> Text -> M a
require body fld name = case body ^? fld of
  Just a -> pure a
  Nothing -> throw $ OtherError $ "Gitea webhook payload missing field: " <> name
