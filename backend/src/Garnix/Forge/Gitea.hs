-- | The Gitea\/Forgejo 'Forge' implementation.
--
-- Gitea has no GitHub-App model: it authenticates API calls with a configured bot
-- access token ('forgeConfigApiToken'), and reports build status via /commit
-- statuses/ rather than check runs. The token-based credential is 'GiteaAuth'.
module Garnix.Forge.Gitea (giteaForge, giteaUserInfo, giteaRepoInfo) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Bool, _String, values)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Garnix.Forge.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import Network.Wreq qualified as Wreq

giteaForge :: (HasCallStack) => Forge 'Gitea
giteaForge =
  Forge
    { _forgeForgeKind = SGitea,
      _forgeGetInstallation = \_ -> GiteaAuth <$> configToken,
      -- Gitea has no app-installation concept; report a synthetic id so
      -- "is garnix authorized" checks succeed for configured instances.
      _forgeGetAppInstallationId = \_ _ -> pure (Just (ForgeInstallationId 0)),
      _forgeGetAccessToken = \auth -> pure (forgeAuthToken auth),
      _forgeGetDefaultBranch = \mAuth owner repo -> do
        tok <- maybe configToken (pure . forgeAuthToken) mAuth
        resp <- giteaGet tok =<< repoUrl owner repo []
        resp' <- checkStatus "getDefaultBranch" resp
        pure $ Branch <$> (resp' ^? Wreq.responseBody . key "default_branch" . _String),
      _forgeGetHeadCommit = \tok owner repo branch -> do
        url <- repoUrl owner repo ["branches", getBranch branch]
        resp <- giteaGet tok url >>= checkStatus "getHeadCommit"
        case resp ^? Wreq.responseBody . key "commit" . key "id" . _String of
          Just sha -> pure (CommitHash sha)
          Nothing -> throw $ OtherError "Gitea getHeadCommit: missing commit.id",
      _forgeNewBuildReport = \frepo report -> do
        postCommitStatus frepo report
        -- Gitea commit statuses have no stable id to update; we just re-post.
        pure (ForgeRunId 0),
      _forgeUpdateBuildReport = \_runId report frepo -> postCommitStatus frepo report,
      _forgeDoesRepoFileExist = \frepo (CommitHash commit) _mFork path -> do
        let tok = forgeAuthToken (_forgeRepoAuth frepo)
        url <- repoUrl (_forgeRepoOwner frepo) (_forgeRepoName frepo) ["contents", cs path]
        resp <- giteaGet tok (url <> "?ref=" <> commit)
        pure $ if resp ^. Wreq.responseStatus . Wreq.statusCode == 200 then FileExists else FileDoesntExist,
      _forgeGetRemote = \frepo _commit mFork -> do
        base <- giteaBaseUrl
        case mFork of
          Just (PrFromFork fork) -> pure $ RemoteUrl $ base <> "/" <> fork <> ".git"
          Nothing -> do
            let tok = getForgeToken (forgeAuthToken (_forgeRepoAuth frepo))
                owner = ownerText (_forgeRepoOwner frepo)
                repo = repoText (_forgeRepoName frepo)
            pure $ RemoteUrl $ embedToken base tok <> "/" <> owner <> "/" <> repo <> ".git",
      _forgeGetRepoCollaborators = \auth owner repo -> do
        let tok = forgeAuthToken auth
        url <- repoUrl owner repo ["collaborators"]
        resp <- giteaGet tok url
        case resp ^. Wreq.responseStatus . Wreq.statusCode of
          404 -> pure RepoNotFound
          code
            | code >= 400 -> throw $ OtherError $ "Gitea getRepoCollaborators: status " <> show code
          _ -> do
            let collaborators =
                  resp ^.. Wreq.responseBody . values . key "login" . _String . to ForgeLogin
            -- Gitea's collaborators list excludes the owner; include it explicitly.
            pure $ Collaborators (getRepoOwner owner : collaborators),
      _forgeGetRepoPublicity = \auth owner repo -> do
        let tok = forgeAuthToken auth
        resp <- giteaGet tok =<< repoUrl owner repo []
        resp' <- checkStatus "getRepoPublicity" resp
        pure $ RepoIsPublic $ not $ fromMaybe False (resp' ^? Wreq.responseBody . key "private" . _Bool),
      -- The following relate to GitHub's app-installation / user-orgs model, which
      -- Gitea doesn't have. They power account-UI breakdowns only; empty is safe.
      _forgeGetInstalledOrgs = \_ -> pure [],
      _forgeGetInstallations = \_ -> pure [],
      _forgeGetReposAccessibleTo = \_ _ -> pure [],
      _forgeOpenPullRequest = \owner repo pr -> do
        tok <- configToken
        url <- repoUrl owner repo ["pulls"]
        let prJson =
              Aeson.object
                [ "title" Aeson..= (pr ^. title),
                  "body" Aeson..= (pr ^. body),
                  "head" Aeson..= getBranch (pr ^. headBranch),
                  "base" Aeson..= getBranch (pr ^. baseBranch)
                ]
        resp <- giteaPost tok url prJson >>= checkStatus "openPullRequest"
        case resp ^? Wreq.responseBody . key "html_url" . _String of
          Just htmlUrl -> pure $ PullRequestResult htmlUrl
          Nothing -> throw $ OtherError "Gitea openPullRequest: missing html_url in response"
    }

-- | Build a 'RepoInfo' for a Gitea repository from a bot API token. Used by the
-- Gitea webhook handler to construct repo context.
giteaRepoInfo :: ForgeToken -> RepoOwner -> RepoName -> RepoInfo
giteaRepoInfo tok owner name =
  RepoInfo (SomeForgeRepo (ForgeRepo giteaForge (GiteaAuth tok) owner name)) owner name

-- | Fetch the authenticated user's login and primary email (for OAuth login).
giteaUserInfo :: (HasCallStack) => ForgeToken -> M (ForgeLogin, Email)
giteaUserInfo tok = do
  base <- giteaBaseUrl
  resp <- giteaGet tok (base <> "/api/v1/user") >>= checkStatus "giteaUserInfo"
  let mLogin = resp ^? Wreq.responseBody . key "login" . _String
      mEmail = resp ^? Wreq.responseBody . key "email" . _String
  case (mLogin, mEmail) of
    (Just l, Just e) -> pure (ForgeLogin l, Email e)
    _ -> throw $ OtherError "Gitea user info: missing login or email"

-- * Helpers

configToken :: (HasCallStack) => M ForgeToken
configToken = do
  cfg <- forgeConfig Gitea
  case forgeConfigApiToken cfg of
    Just tok -> pure tok
    Nothing -> throw $ OtherError "Gitea is configured without an API token"

giteaBaseUrl :: M Text
giteaBaseUrl = stripTrailingSlash . forgeConfigBaseUrl <$> forgeConfig Gitea
  where
    stripTrailingSlash t = fromMaybe t (T.stripSuffix "/" t)

-- | Build a @\/api\/v1\/repos\/{owner}\/{repo}\/...@ URL.
repoUrl :: RepoOwner -> RepoName -> [Text] -> M Text
repoUrl owner repo rest = do
  base <- giteaBaseUrl
  pure $ T.intercalate "/" $ [base, "api", "v1", "repos", ownerText owner, repoText repo] <> rest

ownerText :: RepoOwner -> Text
ownerText = getForgeLogin . getRepoOwner

repoText :: RepoName -> Text
repoText = getRepoName

-- | Insert a token as the userinfo of an @https://@ base URL.
embedToken :: Text -> Text -> Text
embedToken base tok = case T.stripPrefix "https://" base of
  Just rest -> "https://" <> tok <> "@" <> rest
  Nothing -> case T.stripPrefix "http://" base of
    Just rest -> "http://" <> tok <> "@" <> rest
    Nothing -> base

authOpts :: ForgeToken -> Wreq.Options -> Wreq.Options
authOpts (ForgeToken tok) opts =
  opts
    & Wreq.header "Authorization" .~ ["token " <> cs tok]
    & Wreq.checkResponse ?~ \_ _ -> pure ()

giteaGet :: ForgeToken -> Text -> M (Wreq.Response BSL.ByteString)
giteaGet tok url = withWreqOptions $ \opts -> Wreq.getWith (authOpts tok opts) (cs url)

giteaPost :: ForgeToken -> Text -> Aeson.Value -> M (Wreq.Response BSL.ByteString)
giteaPost tok url payload = withWreqOptions $ \opts -> Wreq.postWith (authOpts tok opts) (cs url) payload

checkStatus :: (HasCallStack) => Text -> Wreq.Response BSL.ByteString -> M (Wreq.Response BSL.ByteString)
checkStatus ctx resp = do
  let code = resp ^. Wreq.responseStatus . Wreq.statusCode
  when (code >= 400)
    $ throw
    $ OtherError
    $ "Gitea API error in " <> ctx <> ": status " <> show code <> ": " <> cs (resp ^. Wreq.responseBody)
  pure resp

-- | Post a commit status reflecting a build report.
postCommitStatus :: (HasCallStack) => ForgeRepo 'Gitea -> GhRunReport -> M ()
postCommitStatus frepo report = do
  let tok = forgeAuthToken (_forgeRepoAuth frepo)
      CommitHash sha = report ^. commit
      body =
        Aeson.object
          [ "state" Aeson..= giteaState (report ^. status),
            "context" Aeson..= (report ^. name),
            "description" Aeson..= (report ^. summary),
            "target_url" Aeson..= fromMaybe "" (report ^. url)
          ]
  url' <- repoUrl (_forgeRepoOwner frepo) (_forgeRepoName frepo) ["statuses", sha]
  void $ giteaPost tok url' body >>= checkStatus "postCommitStatus"

giteaState :: RunReportStatus -> Text
giteaState = \case
  RunReportStatusInProgress -> "pending"
  RunReportStatusSuccess -> "success"
  RunReportStatusFailure -> "failure"
  RunReportStatusTimeout -> "error"
  RunReportStatusCancelled -> "error"
