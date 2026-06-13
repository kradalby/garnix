-- Deploy garnix:multi_forge to pg
--
-- Adds a `forge_type` dimension so repos/owners can live on forges other than
-- GitHub. Backward-compatible: every existing row defaults to 'github', and the
-- column is additive so existing (explicit-column) queries are unaffected.

BEGIN;

CREATE TYPE forge_type AS ENUM (
    'github',
    'gitea',
    'gitlab'
);

ALTER TABLE installations            ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE builds                   ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE runs                     ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE commits                  ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE pushes                   ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE users                    ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE action_secrets           ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE repo_config              ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE repo_secrets             ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE repo_owner_has_product   ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE repo_owner_usage_limits  ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE internal_access_tokens   ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE module_user_repo         ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE modules                  ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';
ALTER TABLE cache_store_hash_tags    ADD COLUMN forge_type forge_type NOT NULL DEFAULT 'github';

COMMIT;
