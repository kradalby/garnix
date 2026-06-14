-- | The forge registry: the single boundary where a runtime 'ForgeKind' is turned
-- into a concrete, kind-indexed 'Forge'. Downstream code unpacks the resulting
-- 'SomeForge' once and then works at a statically-known index.
--
-- This is a pure function rather than an 'Env' field because each 'Forge' is a
-- static value; per-deployment configuration (app keys, tokens, …) is pulled from
-- the environment inside @M@ by the individual methods.
module Garnix.Forge
  ( forgeForKind,
    module Garnix.Forge.Types,
  )
where

import Garnix.Forge.Gitea (giteaForge)
import Garnix.Forge.Types
import Garnix.GithubInterface (githubForge)
import Garnix.Monad (SomeForge (..))
import Garnix.Prelude

-- | Resolve a forge by kind. Gitea\/GitLab are not implemented yet (see the
-- multi-forge task list); calling this for them is a deliberate, loud failure
-- until 'Garnix.Forge.Gitea'\/'Garnix.Forge.GitLab' land.
forgeForKind :: ForgeKind -> SomeForge
forgeForKind = \case
  GitHub -> SomeForge githubForge
  Gitea -> SomeForge giteaForge
  GitLab -> error "forgeForKind: the GitLab forge is not implemented yet"
