-- | Forge-neutral, pure (monad-free) types underpinning multi-forge support.
--
-- The forge /kind/ is a type-level index ('ForgeKind' promoted via @DataKinds@) so
-- that an interface, its credentials, and the repos that use it all carry the same
-- @k@. Crucially, each 'ForgeAuth' constructor returns a /distinct concrete/ index,
-- so a credential for one forge can never be fed to another's implementation — that
-- mismatch is unrepresentable rather than merely discouraged.
--
-- This module is deliberately free of the application monad @M@. The @M@-returning
-- @Forge@ record (the generalized GitHub interface) lives in "Garnix.Monad", which
-- imports this module and reuses the report\/collaborator\/PR types defined there.
-- Forge-neutral identifier types (@ForgeToken@, @RepoOwner@, …) live in
-- "Garnix.Types".
module Garnix.Forge.Types
  ( -- * Forge kinds
    ForgeKind (..),
    SForgeKind (..),
    SomeSForgeKind (..),
    fromSForgeKind,
    toSForgeKind,
    forgeKindSlug,

    -- * Identifiers
    ForgeInstallationId (..),

    -- * Credentials
    ForgeAuth (..),
    forgeAuthToken,

    -- * Per-forge configuration
    ForgeConfig (..),
    GithubAppConfig (..),
  )
where

import Data.ByteString (ByteString)
import Garnix.Prelude
import Garnix.Types (ForgeToken)
import GitHub.App.Auth qualified as GHA
import GitHub.Data (Id)
import GitHub.Data.Apps (App)

-- | The set of supported forges. The /term-level/ value is what gets persisted to
-- the DB (@forge_type@ enum), used for webhook routing and JSON; the /type-level/
-- index (via @DataKinds@) is what enforces forge\/credential matching.
data ForgeKind = GitHub | Gitea | GitLab
  deriving stock (Eq, Show, Read, Ord, Enum, Bounded, Generic)

-- | Stable lowercase identifier used in URLs, the DB enum and JSON.
forgeKindSlug :: ForgeKind -> Text
forgeKindSlug = \case
  GitHub -> "github"
  Gitea -> "gitea"
  GitLab -> "gitlab"

-- | Singleton for 'ForgeKind': lets us recover the type-level kind from a runtime
-- value (and vice versa) when crossing the existential boundary.
data SForgeKind (k :: ForgeKind) where
  SGitHub :: SForgeKind 'GitHub
  SGitea :: SForgeKind 'Gitea
  SGitLab :: SForgeKind 'GitLab

deriving stock instance Show (SForgeKind k)

deriving stock instance Eq (SForgeKind k)

fromSForgeKind :: SForgeKind k -> ForgeKind
fromSForgeKind = \case
  SGitHub -> GitHub
  SGitea -> Gitea
  SGitLab -> GitLab

-- | Existential wrapper recovering the type-level index from a runtime 'ForgeKind'.
data SomeSForgeKind where
  SomeSForgeKind :: SForgeKind k -> SomeSForgeKind

toSForgeKind :: ForgeKind -> SomeSForgeKind
toSForgeKind = \case
  GitHub -> SomeSForgeKind SGitHub
  Gitea -> SomeSForgeKind SGitea
  GitLab -> SomeSForgeKind SGitLab

-- | A forge's id for an app installation\/integration association. For GitHub this
-- is the App installation id; forges without that concept return a synthetic id.
newtype ForgeInstallationId = ForgeInstallationId {getForgeInstallationId :: Integer}
  deriving stock (Eq, Show, Ord, Generic)

-- | Per-forge credentials. Each constructor pins /one/ concrete kind index — there
-- is intentionally no @ForgeAuth k@ catch-all — so @GiteaAuth :: ForgeAuth 'Gitea@
-- and @GitLabAuth :: ForgeAuth 'GitLab@ are distinct types even though both wrap a
-- bare token. GitHub additionally carries the installation auth needed to mint app
-- tokens.
data ForgeAuth (k :: ForgeKind) where
  GithubAuth :: GHA.InstallationAuth -> ForgeToken -> ForgeAuth 'GitHub
  GiteaAuth :: ForgeToken -> ForgeAuth 'Gitea
  GitLabAuth :: ForgeToken -> ForgeAuth 'GitLab

-- | GitHub-App-specific configuration (only GitHub has an "App" model).
data GithubAppConfig = GithubAppConfig
  { githubAppConfigAuth :: GHA.AppAuth,
    githubAppConfigName :: Text,
    githubAppConfigId :: Id App
  }

-- | Per-forge configuration\/secrets. One of these exists per /configured/ forge;
-- forges without configuration are simply absent from the registry.
data ForgeConfig = ForgeConfig
  { -- | API\/clone host (e.g. github.com, a Gitea host, a GitLab host).
    forgeConfigBaseUrl :: Text,
    -- | Secret used to verify inbound webhook signatures.
    forgeConfigWebhookSecret :: ByteString,
    -- | OAuth client id for user login.
    forgeConfigOAuthClientId :: Text,
    -- | OAuth client secret for user login.
    forgeConfigOAuthClientSecret :: Text,
    -- | A bot API token used to call the forge's API (post statuses, open PRs, …).
    -- Used by token-based forges (Gitea, GitLab); GitHub uses app installation
    -- tokens instead, so this is 'Nothing' there.
    forgeConfigApiToken :: Maybe ForgeToken,
    -- | GitHub App config; present only for GitHub.
    forgeConfigApp :: Maybe GithubAppConfig
  }

-- | The access token carried by any credential, regardless of forge.
forgeAuthToken :: ForgeAuth k -> ForgeToken
forgeAuthToken = \case
  GithubAuth _ t -> t
  GiteaAuth t -> t
  GitLabAuth t -> t

-- | Redacting 'Show' so credentials never leak into logs. Defined via 'showsPrec'
-- because the project prelude hides the 'show' class method.
instance Show (ForgeAuth k) where
  showsPrec _ a = showString $ case a of
    GithubAuth {} -> "GithubAuth <installation> <token>"
    GiteaAuth {} -> "GiteaAuth <token>"
    GitLabAuth {} -> "GitLabAuth <token>"
