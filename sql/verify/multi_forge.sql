-- Verify garnix:multi_forge on pg

BEGIN;

-- The enum type exists.
SELECT 'github'::forge_type;

-- A representative set of tables have the forge_type column.
SELECT forge_type FROM installations WHERE FALSE;
SELECT forge_type FROM builds WHERE FALSE;
SELECT forge_type FROM runs WHERE FALSE;
SELECT forge_type FROM commits WHERE FALSE;
SELECT forge_type FROM pushes WHERE FALSE;
SELECT forge_type FROM users WHERE FALSE;

ROLLBACK;
