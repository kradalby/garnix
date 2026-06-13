-- Revert garnix:multi_forge from pg

BEGIN;

ALTER TABLE installations            DROP COLUMN forge_type;
ALTER TABLE builds                   DROP COLUMN forge_type;
ALTER TABLE runs                     DROP COLUMN forge_type;
ALTER TABLE commits                  DROP COLUMN forge_type;
ALTER TABLE pushes                   DROP COLUMN forge_type;
ALTER TABLE users                    DROP COLUMN forge_type;
ALTER TABLE action_secrets           DROP COLUMN forge_type;
ALTER TABLE repo_config              DROP COLUMN forge_type;
ALTER TABLE repo_secrets             DROP COLUMN forge_type;
ALTER TABLE repo_owner_has_product   DROP COLUMN forge_type;
ALTER TABLE repo_owner_usage_limits  DROP COLUMN forge_type;
ALTER TABLE internal_access_tokens   DROP COLUMN forge_type;
ALTER TABLE module_user_repo         DROP COLUMN forge_type;
ALTER TABLE modules                  DROP COLUMN forge_type;
ALTER TABLE cache_store_hash_tags    DROP COLUMN forge_type;

DROP TYPE forge_type;

COMMIT;
